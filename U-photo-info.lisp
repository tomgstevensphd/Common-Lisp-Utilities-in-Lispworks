;;************************** U-photo-info.lisp **************************
;;
;;
;; CODE SOURCE -- SEE BELOW


;;Original copy in C:\TOM\LISP EXAMPLES-CODE\Photo Data xpb-exif

;;;ZPB-EXIF - access to digital image Exif data for Common Lisp




#|
ABSTRACT

Exif is a standard for embedding information in an image file created by a digital camera. ZPB-EXIF is a library that makes Exif data accessible to Common Lisp programs. It is available under a BSD-like license. For more info about Exif, see Exif.org. The latest version is 1.2.2, released on September 14th, 2012.

Download shortcut: http://www.xach.com/lisp/zpb-exif.tgz
|#

;;
;;Must use this package, or signals error bec tries to redefine symbol type. OR NOT?
;; WHEN USING THESE FUNCTIONS EITHER IMPORT OR USE PACKAGE NAME  ZPB-EXIF:function-name

#|
CONTENTS

Overview
Example
Terms
The ZPB-EXIF Dictionary
MAKE-EXIF
exif-value
PARSED-EXIF-VALUE
EXIF-ALIST
ifd-alist
ifd-entry
entry-value
PARSED-ENTRY-VALUE
image-ifd
exif-ifd
gps-ifd
interoperability-ifd
thumbnail-ifd
exif-data
thumbnail-image

invalid-stream
invalid-jpeg-stream
invalid-exif-stream
Parsed Values
Image IFD
Exif IFD
Feedback


-------------------------- OVERVIEW -------------------------------------------------

EXIF DATA IS STRUCTURED LIKE SO:

1. data about an image is separated into multiple sections called IMAGE FILE DIRECTORIES or IFDs
2. each IFD contains one or more KEY/VALUE PAIRs called IFD entries; the key is also called the TAG
3.there are two MANDATORY IFDs for an Exif image: the IMAGE IFD and the EXIF IFD
4.there are a number of OPTIONAL IFDs: the GPS IFD, the interoperability IFD, the thumbnail IFD
5.the thumbnail IFD may also have separate GPS and interoperability IFDs, but these are not supported by ZPB-EXIF
6. ZPB-EXIF provides access to key/value lookup through an object created from an EXIF FILE OR STREAM. It provides quick access to image, Exif, and GPS IFD values, and indirect access to other IFDs and their key/value entries.

EXAMPLE ------------------------------------------------------------------

> (defparameter *exif* (make-exif #p"gg_gps.JPG"))
=> *EXIF*

> (EXIF-VALUE "FNumber" *exif*)
=> 9/2

> (parsed-exif-value "MeteringMode" *exif*)
=> :PATTERN

;;XXX
> (exif-value :DateTimeOriginal *exif*)
=> "2003:11:23 18:07:37"

> (exif-value #x0112 *exif*)   ; #x0112 => orientation tag in the image IFD
=> 6

> (parsed-exif-value #x0112 *exif*)
=> :ROTATED-270

> (exif-value :ISOSpeedRating *exif*)   ; Field not present in this particular image
=> NIL

> (list (exif-value "GPSLatitude" *exif*)
        (exif-value "GPSLatitudeRef" *exif*)
        (exif-value "GPSLongitude" *exif*)
        (exif-value "GPSLongitudeRef" *exif*))
=> (#(39 54 56) "N" #(116 23 27) "E")

MY EXAMPLE (see at bottom of file) ------------------------------------------------------------------
|#

#|
----------------------------------------------------------------------------------------


;;XXX
;;
------------------------------------------- TERMS ----------------------------------------

TAG
an integer that refers to a unique field in a specific IFD. For example, the integer #x0001 represents the InteroperabilityIndex field in the interoperability IFD and the GPSVersionID field in the GPS IFD
tag designator
an object representing a tag and that is one of: a string (denoting a tag named by the string for a given IFD), a symbol (denoting a tag named by the string that is its name), or a tag (denoting itself)
The ZPB-EXIF Dictionary

--------------------------------------------- FUNCTIONS -----------------------------------------
;;XXX
[Function]
MAKE-EXIF exif-stream-designator => exif

RETURNS AN EXIF OBJECT.
EXIF-STREAM-DESIGNATOR may be a stream (designating itself), a pathname (designating the stream created by opening the file), or a STRING (treated the same as a pathname).
 If exif-stream-designator does not refer to a JPEG, signals an invalid-jpeg-stream error. If exif-stream-designator refers to a JPEG, but the JPEG does not contain any Exif data, signals an invalid-exif-stream error.

;;XXX
[Function]
EXIF-VALUE tag-designator exif => value

Returns the VALUE OF THE FIELD identified by TAG-DESIGNATOR from the image IFD, Exif IFD, or GPS IFD.

;;XXX
[Function]
PARSED-EXIF-VALUE tag-designator exif => parsed-value

Returns an the value of the field identified by tag-designator from the image IFD, Exif IFD, or GPS IFD in PARSED FORM.

;;xxx
[Function]
EXIF-ALIST exif &key parsedp => alist

Returns an ASSOCIATION LIST that has tag names for keys and IFD entry values for values for ALL ENTRIES IN THE IMAGE, Exif, and GPS IFDs in exif. If parsedp is true, the IFD entry values are in parsed form.

[Function]
IFD-ALIST ifd &key parsedp => alist

Returns an association list that has tag names for keys and IFD entry values for values FOR THE ENTRIES IFD. If parsedp is true, the IFD entry values are in PARSED FORM.

;;
;;XXX -- USE THIS ONE THAT RETURNS NIL INSTEAD OF ERROR IF NO VALUE
[Function]
IFD-ENTRY tag-designator ifd => ifd-entry

Returns the IFD entry identified by tag-designator in ifd,
 or NIL if no IFD entry could be found.


[Function]
entry-value ifd-entry => value

Returns the value of ifd-entry.

[Function]
parsed-entry-value ifd-entry => value

Returns the value of ifd-entry in parsed form.

[Function]
image-ifd exif => ifd

Returns the image IFD for exif. The following tags may be available in the image IFD:

Tag	Name	Data Type
TAGS RELATING TO IMAGE DATA STRUCTURE
;;XXX
#x0100	ImageWidth	short or long
#x0101	ImageHeight	short or long
#x0102	BitsPerSample	short
#x0103	Compression	short
#x0106	PhotometricInterpretation	short
;;XXX
#x0112	Orientation	short
#x0115	SamplesPerPixel	short
#x011C	PlanarConfiguration	short
#x0212	YCbCrSubSampling	short
#x0213	YCbCrPositioning	short
;;XXX
#x011A	XResolution	rational
#x011B	YResolution	rational
#x0128	ResolutionUnit	short

TAGS RELATING TO RECORDING OFFSETS
#x0111	StripOffsets	short or long
#x0116	RowsPerStrip	short or long
#x0117	StripByteCounts	short or long
#x0201	JPEGInterchangeFormat	long
#x0202	JPEGInterchangeFormatLength	long

TAGS RELATING TO IMAGE DATA CHARACTERISTICS
#x012D	TransferFunction	short
#x013E	WhitePoint	rational
#x013F	PrimaryChromaticities	rational
#x0211	YCbCrCoefficients	rational; parsed value available
#x0214	ReferenceBlackWhite	rational

OTHER TAGS
;;XXX
#x0132	DateTime	ascii; parsed value available
#x010E	ImageDescription	ascii

#x010F	Make	ascii
#x0110	Model	ascii
#x0131	Software	ascii
#x013B	Artist	ascii
#x8298	Copyright	ascii

;;XXX
[Function]
EXIF-IFD exif => ifd

RETURNS THE EXIF IFD FOR EXIF. THE FOLLOWING TAGS MAY BE AVAILABLE IN THE EXIF IFD:

Tag	Name	Data Type

TAGS RELATING TO VERSION
#x9000	ExifVersion	undefined; parsed value available
#xA000	FlashpixVersion	undefined
Tags Relating to Image Data Characteristics
#xA001	ColorSpace	short; parsed value available
#xA001	Gamma	rational; parsed value available
#x9101	ComponentsConfiguration	undefined
#x9102	CompressedBitsPerPixel	rational
#xA002	PixelXDimension	short or long
#xA003	PixelYDimension	short or long

TAGS RELATING TO USER INFORMATION
#x927C	MakerNote	undefined
#x9286	UserComment	undefined; parsed value available
#xA004	RelatedSoundFile	ascii

;;XXX
TAGS RELATING TO DATE AND TIME
#x9003	DateTimeOriginal	ascii; parsed value available
#x9004	DateTimeDigitized	ascii; parsed value available

#x9290	SubSecTime	ascii
#x9291	SubSecTimeOriginal	ascii
#x9292	SubSecTimeDigitized	ascii

TAGS RELATING TO PICTURE TAKING CONDITIONS
#x829A	ExposureTime	rational; parsed value available
#x829D	FNumber	rational; parsed value available
#x8822	ExposureProgram	rational; parsed value available
#x8824	SpectralSensitivity	ascii
#x8827	ISOSpeedRatings	short
#x8828	OECF	undefined
#x9201	ShutterSpeedValue	srational; parsed value available
#x9202	ApertureValue	rational; parsed value available
#x9203	BrightnessValue	srational
#x9204	ExposureBiasValue	srational
#x9205	MaxApertureValue	rational; parsed value available
#x9206	SubjectDistance	rational; parsed value available
#x9207	MeteringMode	short; parsed value available
#x9208	LightSource	short; parsed value available
#x9209	Flash	short; parsed value available
#x920A	FocalLength	rational; parsed value available
#x9214	SubjectArea	short
#xA20B	FlashEngergy	rational
#xA20C	SpatialFrquencyResponse	undefined
#xA20E	FocalPlaneXResolution	rational; parsed value available
#xA20F	FocalPlaneYResolution	rational; parsed value available
#xA210	FocalPlaneResolutionUnit	short
#xA214	SubjectLocation	short
#xA215	ExposureIndex	rational
#xA217	SensingMethod	short; parsed value available
#xA300	FileSource	undefined
#xA301	SceneType	undefined
#xA302	CFAPattern	undefined
#xA401	CustomRendered	short
#xA402	ExposureMode	short; parsed value available
#xA403	WhiteBalance	short; parsed value available
#xA404	DigitalZoomRatio	rational
#xA405	FocalLengthIn35mmFilm	short
#xA406	SceneCaptureType	short; parsed value available
#xA407	GainControl	rational; parsed value available
#xA408	Contrast	short; parsed value available
#xA409	Saturation	short; parsed value available
#xA40A	Sharpness	short; parsed value available
#xA40B	DeviceSettingDescription	undefined
#xA40C	SubjectDistanceRange	short
#xA420	ImageUniqueID	ascii

;;XXX
[Function]
GPS-IFD exif => ifd

Returns the GPS IFD for exif.
The following tags are available in the GPS IFD:
Tag	Name	Data Type
#x0000	GPSVersionID	byte
#x0001	GPSLatitudeRef	ascii
#x0002	GPSLatitude	rational
#x0003	GPSLongitudeRef	ascii
#x0004	GPSLongitude	ascii
#x0005	GPSAltitudeRef	byte
#x0006	GPSAltitude	rational
#x0007	GPSTimeStamp	rational
#x0008	GPSSatellites	ascii
#x0009	GPSStatus	ascii
#x000A	GPSMeasureMode	ascii
#x000B	GPSDOP	rational
#x000C	GPSSpeedRef	ascii
#x000D	GPSSpeed	rational
#x000E	GPSTrackRef	ascii
#x000F	GPSTrack	rational
#x0010	GPSImgDirectionRef	ascii
#x0011	GPSImgDirection	rational
#x0012	GPSMapDatum	ascii
#x0013	GPSDestLatitudeRef	ascii
#x0014	GPSDestLatitude	rational
#x0015	GPSDestLongitudeRef	ascii
#x0016	GPSDestLongitude	rational
#x0017	GPSDestBearingRef	ascii
#x0018	GPSDestBearing	rational
#x0019	GPSDestDistanceRef	ascii
#x001A	GPSDestDistance	rational
[Function]
interoperability-ifd exif => ifd

Returns the interoperability IFD for exif.

;;XXX
[Function]
THUMBNAIL-IFD exif => ifd

Returns the thumbnail IFD for exif. The tags available are the same as for the image IFD.
[Function]
exif-data exif => value

Returns a vector of (unsigned-byte 8) data representing all the data in the Exif section of the stream from which exif was created.
[Function]
thumbnail-image exif => value

Returns a vector of (unsigned-byte 8) data representing the embedded Exif thumbnail of exif, or NIL if no Exif thumbnail is present.
[Condition]
invalid-stream


-------------------  ERROR CONDITIONS ------------------
This error condition is a supertype of Exif stream conditions.
[Condition]
INVALID-JPEG-STREAM

This condition is signalled when a stream being processed is not a JPEG stream. It is a subtype of invalid-stream.
[Condition]
INVALID-EXIF-STREAM

This condition is signalled when a stream being processed is not an Exif stream. It is a subtype of invalid-stream.
Parsed Values

When using parsed-exif-value, the following IFD/tag combinations are converted from the "raw" Exif value into a more processing-friendly format. All others return the same value that would be returned by exif-value

IMAGE IFD --------------------------------------------------

YCbCrSubsampling: returns one of :CENTERED, :CO-SITED, or :RESERVED
DateTime: returns a universal time
Exif IFD

ExifVersion: returns a string representing the version, e.g. "2.1", "2.2", "2.21"
ColorSpace: returns one of :SRGB, :UNCALIBRATED, or :RESERVED
Gamma: returns a float
UserComment: if the UserComment is in ASCII or Undefined format, returns a string representing the comment
;;XXX
DateTimeOriginal: returns a universal time
DateTimeDigitized: returns a universal time
ExposureTime: returns a float
FNumber: returns a float
ExposureProgram: returns one of :NOT-DEFINED, :MANUAL, :NORMAL-PROGRAM, :APERTURE-PRIORITY, :SHUTTER-PRIORITY, :CREATIVE-PROGRAM, :ACTION-PROGRAM, :PORTRAIT-MODE, :LANDSCAPE-MODE, or :RESERVED
ShutterSpeedValue: returns a float
ApertureValue: returns a float
MaxApertureValue: returns a float
SubjectDistance: returns a float
MeteringMode: returns one of :UNKNOWN, :AVERAGE, :CENTER-WEIGHTED-AVERAGE, :SPOT, :MULTISPOT, :PATTERN, :PARTIAL, :OTHER, or :RESERVED
LightSource: returns one of :UNKNOWN, :DAYLIGHT, :FLOURESCENT, :TUNGSTEN, :FLASH, :FINE-WEATHER, :CLOUDY-WEATHER, :SHADE, :DAYLIGHT-FLOURESCENT, :DAY-WHITE-FLOURESCENT, :COOL-WHITE-FLOURESCENT, :WHITE-FLOURESCENT, :STANDARD-LIGHT-A, :STANDARD-LIGHT-B, :STANDARD-LIGHT-C, :D55, :D65, :D75, :D50, :ISO-STUDIO-TUNGSTEN, :OTHER-LIGHT-SOURCE, or :RESERVED.
Flash: returns T if the flash fired
FocalLength: returns a float
FocalPlaneXResolution: returns a float
FocalPlaneYResolution: returns a float
SensingMethod: returns one of :NOT-DEFINED, :ONE-CHIP-COLOR-AREA-SENSOR, :TWO-CHIP-COLOR-AREA-SENSOR, :THREE-CHIP-COLOR-AREA-SENSOR, :COLOR-SEQUENTIAL-AREA-SENSOR, :TRILINEAR-SENSOR, :COLOR-SEQUENTIAL-LINEAR-SENSOR, or :RESERVED
ExposureMode: returns one of :AUTO-EXPOSURE, :MANUAL-EXPOSURE, :AUTO-BRACKET, or :RESERVED
WhiteBalance: returns one of :AUTO, :MANUAL, or :RESERVED
SceneCaptureType: returns one of :STANDARD, :LANDSCAPE, :PORTRAIT, :NIGHT-SCENE, or :RESERVED
Contrast: returns one of :NORMAL, :SOFT, :HARD, or :RESERVED
Saturation: returns one of :NORMAL, :LOW-SATURATION, :HIGH-SATURATION, or :RESERVED
Sharpness: returns one of :NORMAL, :SOFT, :HARD, or :RESERVED
Feedback

This library was created by Zachary Beane. For questions, comments, bug reports, and patches, please email him.

$Header: /home/cvs/zpb-exif/zpb-exif.html,v 1.5 2009/09/08 17:16:34 xach Exp $

|#

;;; 
;;; exif.lisp
;;; 
;;; Created: 2005-12-08 by Zach Beane <xach@xach.com>
;;; 
;;; Copyright (c) 2005 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; $Id: exif.lisp,v 1.7 2009/09/08 17:14:22 xach Exp $

;;SSS START LISP CODE HERE
;;
;;DDD
;;
;;IF DON'T USE THIS PACKAGE, GET ERROR BEC REDEFINES 'TYPE
;;
(defpackage :zpb-exif
  (:use :cl)
  (:export
            :make-exif
           ;; IFDs
           :image-ifd
           :thumbnail-ifd
           :exif-ifd
           :gps-ifd
           :interoperability-ifd
           ;; Pulling values from IFDs and entries
           :ifd-entry
           :entry-value
           :parsed-entry-value
           ;; Convenience
           :exif-value
           :parsed-exif-value
           :parse-exif-data
           :exif-data
           :thumbnail-image
           :ifd-alist
           :exif-alist
           ;; Conditions
           :invalid-stream
           :invalid-jpeg-stream
           :invalid-exif-stream)
  (:shadow count type))

(in-package :zpb-exif)



;;; Conditions

(define-condition invalid-stream (error) ())

(define-condition invalid-jpeg-stream (invalid-stream) ())

(define-condition invalid-exif-stream (invalid-stream) ())
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; (defparameter *optimizations*
   ;;  '(optimize (speed 3) (safety 0)))

  (defparameter *optimizations* '(optimize)))


;;; Fetching data

(defun get-32/lsb (pos buf)
  (declare (cl:type (simple-array (unsigned-byte 8) (*)) buf)
           (fixnum pos)
           #.*optimizations*)
  (logand #xFFFFFFFF
          (+ (ash (aref buf pos) 0)
             (ash (aref buf (incf pos)) 8)
             (ash (aref buf (incf pos)) 16)
             (ash (aref buf (incf pos)) 24))))

(defun get-32/msb (pos buf)
  (declare (cl:type (simple-array (unsigned-byte 8) (*)) buf)
           (fixnum pos)
           #.*optimizations*)  
  (logand #xFFFFFFFF
          (+ (ash (aref buf pos) 24)
             (ash (aref buf (incf pos)) 16)
             (ash (aref buf (incf pos)) 8)
             (ash (aref buf (incf pos)) 0))))

(defun get-16/lsb (pos buf)
  (declare (cl:type (simple-array (unsigned-byte 8) (*)) buf)
           (fixnum pos)
           #.*optimizations*)  
  (logand #xFFFF
          (+ (ash (aref buf pos) 0)
             (ash (aref buf (incf pos)) 8))))

(defun get-16/msb (pos buf)
  (declare (cl:type (simple-array (unsigned-byte 8) (*)) buf)
           (fixnum pos)
           #.*optimizations*)
  (logand #xFFFF
          (+ (ash (aref buf pos) 8)
             (ash (aref buf (incf pos)) 0))))

(defun get-8/* (pos buf)
  (declare (cl:type (simple-array (unsigned-byte 8) (*)) buf)
           (fixnum pos)
           #.*optimizations*)
  (logand #xFF (aref buf pos)))


;;; The exif object

(defclass exif ()
  ((file
    :initarg :file
    :initform nil
    :reader file)
   (data
    :initarg :data
    :reader data)
   (offset
    :initarg :offset
    :reader offset
    :documentation
    "The position in the input stream from where the Exif data was read.")
   (get-32-function
    :initarg :get-32-function
    :reader get-32-function)
   (get-16-function
    :initarg :get-16-function
    :reader get-16-function)
   (endianness
    :initarg :endianness
    :reader endianness)
   (image-ifd
    :initarg :image-ifd
    :initform nil
    :accessor image-ifd)
   (thumbnail-ifd
    :initarg :thumbnail-ifd
    :initform nil
    :accessor thumbnail-ifd)
   (exif-ifd
    :initarg :exif-ifd
    :initform nil
    :accessor exif-ifd)
   (gps-ifd
    :initarg :gps-ifd
    :initform nil
    :accessor gps-ifd)
   (interoperability-ifd
    :initarg :interoperability-ifd
    :initform nil
    :accessor interoperability-ifd)))
    
(defun exif-data (exif)
  (data exif))

(defun get-8 (pos exif)
  (get-8/* pos (data exif)))

(defun get-16 (pos exif)
  (funcall (get-16-function exif) pos (data exif)))

(defun get-32 (pos exif)
  (funcall (get-32-function exif) pos (data exif)))

(defun get-string (pos length exif)
  (let ((data (data exif))
        (string (make-string (1- length))))
    (loop for i from pos
          for j from 0
          repeat (1- length)
          do (setf (char string j) (code-char (aref data i))))
    string))


;;; Reading the various data types

(defun long->slong (long)
  (if (logbitp 31 long)
      (dpb long (byte 32 0) -1)
      long))

(defmacro do-gets ((j start step count) &body body)
  "Evaluate BODY COUNT times, binding J to START initially, then
incrementing it by STEP. The results of the evaluation are returned as
a vector."
  (let ((result (gensym))
        (i (gensym)))
    `(do ((,result (make-array ,count))
          (,i 0 (1+ ,i ))
          (,j ,start (+ ,j ,step)))
         ((= ,i ,count) ,result)
      (setf (aref ,result ,i)
       ,@body))))

(defun get-byte (type count pos exif)
  (declare (ignore type))
  (if (= count 1)
      (get-8 pos exif)
      (subseq (data exif) pos (+ pos count))))

(defun get-ascii (type count pos exif)
  (declare (ignore type))
  (if (plusp count)
      (get-string pos count exif)
      ""))

(defun get-short (type count pos exif)
  (declare (ignore type))
  (if (= count 1)
      (get-16 pos exif)
      (do-gets (i pos 2 count)
        (get-16 i exif))))

(defun get-long (type count pos exif)
  (declare (ignore type))
  (if (= count 1)
      (get-32 pos exif)
      (do-gets (i pos 4 count)
        (get-32 i exif))))

(defun get-rational (type count pos exif)
  (declare (ignore type))
  (if (= count 1)
      (let ((numerator (get-32 pos exif))
            (denominator (get-32 (+ 4 pos ) exif)))
        (if (zerop denominator)
            0
            (/ numerator denominator)))
      (do-gets (i pos 8 count)
        (let ((numerator (get-32 i exif))
              (denominator (get-32 (+ 4 i) exif)))
          (if (zerop denominator)
              0
              (/ numerator denominator))))))

(defun get-undefined (type count pos exif)
  (declare (ignore type))
  (subseq (data exif) pos (+ pos count)))

(defun get-slong (type count pos exif)
  (declare (ignore type))
  (if (= count 1)
      (long->slong (get-32 pos exif))
      (do-gets (i pos 4 count)
        (long->slong (get-32 i exif)))))

(defun get-srational (type count pos exif)
  (declare (ignore type))
  (if (= count 1)
      (/ (long->slong (get-32 pos exif))
         (long->slong (get-32 (+ pos 4) exif)))
      (do-gets (i pos 8 count)
        (/ (long->slong (get-32 i exif))
           (long->slong (get-32 (+ i 4) exif))))))

(defun get-unknown-type (type &rest args)
  (declare (ignore args))
  (warn "Encountered unknown data type ~D, ignoring" type)
  nil)

(defparameter *type-readers*
  #(get-unknown-type          ; 0
    get-byte                  ; 1
    get-ascii                 ; 2
    get-short                 ; 3
    get-long                  ; 4
    get-rational              ; 5
    get-unknown-type          ; 6
    get-undefined             ; 7
    get-unknown-type          ; 8
    get-slong                 ; 9
    get-srational             ;10
    get-unknown-type          ;11
    get-unknown-type          ;12
    get-unknown-type          ;13
    get-unknown-type          ;14
    get-unknown-type          ;15
    ))


;;; IFDs

(defclass ifd ()
  ((tagset
    :initarg :tagset
    :accessor tagset)
   (entries
    :initarg :entries
    :accessor entries)
   (next-ifd-offset
    :initarg :next-ifd-offset
    :initform 0
    :accessor next-ifd-offset)))

(defmethod print-object ((ifd ifd) stream)
  (print-unreadable-object (ifd stream :type t :identity t)
    (format stream "(~D entries)" (length (entries ifd)))))

(defclass ifd-entry ()
  ((exif
    :initarg :exif
    :reader exif)
   (ifd
    :initarg :ifd
    :reader ifd)
   (tag
    :initarg :tag
    :reader tag)
   (type
    :initarg :type
    :reader type)
   (count
    :initarg :count
    :reader count)
   (value-offset
    :initarg :value-offset
    :reader value-offset)
   (value
    :initarg :value
    :reader %value
    :writer (setf value))))

(defun immediate-value-p (type count)
  "Can COUNT items of TYPE be stored in the 32-bit IFD entry value
offset area?"
  (case type
    ;; octet-sized types
    ((1 2 7) (<= count 4))
    ;; 16-bit type
    (3 (< count 2))
    ;; 32-bit types
    ((4 9) (= count 1))))

(defun read-ifd-value (type count pos exif)
  (funcall (aref *type-readers* type) type count pos exif))

(defun read-ifd-entry (pos ifd exif)
  (let ((tag (get-16 pos exif))
        (type (get-16 (+ pos 2) exif))
        (count (get-32 (+ pos 4) exif))
        (value-offset (get-32 (+ pos 8) exif)))
    (let ((ifd-entry (make-instance 'ifd-entry
                                    :exif exif
                                    :ifd ifd
                                    :tag tag
                                    :type type
                                    :count count
                                    :value-offset value-offset)))
      (when (immediate-value-p type count)
        (setf (value ifd-entry) (read-ifd-value type count (+ pos 8) exif)))
      ifd-entry)))

(defun read-ifd (pos tagset exif)
  (let* ((entry-count (get-16 pos exif))
         (entries (make-array entry-count))
         (next-ifd-offset (get-32 (+ pos 2 (* 12 entry-count)) exif))
         (ifd (make-instance 'ifd
                             :tagset tagset
                             :entries entries
                             :next-ifd-offset next-ifd-offset)))
    (loop for i below entry-count
          for j from 2 by 12
          do (setf (aref entries i) (read-ifd-entry (+ pos j) ifd exif)))
    ifd))

(defun initialize-ifd-entry (ifd-entry)
  (with-slots (exif type count value-offset)
      ifd-entry
    (setf (value ifd-entry) (read-ifd-value type count value-offset exif))))

(defun entry-value (ifd-entry)
  (when ifd-entry
    (if (slot-boundp ifd-entry 'value)
        (%value ifd-entry)
        (initialize-ifd-entry ifd-entry))))

(defun bisect-find (object vector &key (key #'identity))
  (let ((lo 0)
        (hi (length vector))
        (mid nil))
    (loop
     (when (<= hi lo)
       (return))
     (setf mid (ash (+ hi lo) -1))
     (let ((candidate (funcall key (aref vector mid))))
       (cond ((= candidate object)
              (return (aref vector mid)))
             ((< object candidate)
              (setf hi mid))
             (t
              (setf lo (1+ mid))))))))

(defun %ifd-entry (tag ifd)
  (when ifd
    (bisect-find tag (entries ifd) :key #'tag)))


;;;
;;; Initializing IFDs in the exif
;;;
;;; An IFD is a directory of tag/value structures. Exif defines
;;; separate IFD tags for image information, exif-specific
;;; information, GPS information, and interoperability
;;; information. There may be image, exif, and GPS IFDs for both the
;;; primary image and the thumbnail image.
;;;
;;; The image information IFD has pointers to exif and GPS IFDs. The
;;; exif IFD has a pointer to the interoperability IFD.
;;;
;;; FIXME: GPS and Exif IFDs for the thumbnail image are ignored.
;;;

(defun initialize-exif-ifds (exif)
  (let ((image-ifd-offset (get-32 4 exif)))
    (with-slots (image-ifd thumbnail-ifd exif-ifd
                 gps-ifd interoperability-ifd)
        exif
      (setf image-ifd (read-ifd image-ifd-offset 'image exif))
      (unless (zerop (next-ifd-offset image-ifd))
        (setf thumbnail-ifd (read-ifd (next-ifd-offset image-ifd)
                                      'image
                                      exif)))
      (let ((exif-ifd-offset (entry-value (%ifd-entry #x8769 image-ifd)))
            (gps-ifd-offset (entry-value (%ifd-entry #x8825 image-ifd))))
        (when exif-ifd-offset
          (setf exif-ifd (read-ifd exif-ifd-offset 'exif exif))
          (let ((interoperability-ifd-offset (entry-value (%ifd-entry
                                                           #xA005
                                                           exif-ifd))))
            (when interoperability-ifd-offset
              (setf interoperability-ifd (read-ifd interoperability-ifd-offset
                                                   'interoperability
                                                   exif)))))
        (when gps-ifd-offset
          (setf gps-ifd (read-ifd gps-ifd-offset 'gps exif)))
        exif))))

        


;;; Creating an exif object from a stream

(defparameter *reader-functions/msb*
  '(get-32/msb
    get-16/msb))

(defparameter *reader-functions/lsb*
  '(get-32/lsb
    get-16/lsb))


(defun read-jpeg-uint16 (stream)
  (logand #xFFFF
          (+ (ash (read-byte stream) 8)
             (read-byte stream))))
;;mmm
(defun check-bytes (stream &rest bytes)
  "Return true if next bytes of STREAM match the list BYTES."
  (loop for byte in bytes
        always (= (read-byte stream) byte)))
;;original
#|(defun check-bytes (stream &rest bytes)
  "Return true if next bytes of STREAM match the list BYTES."
  (loop for byte in bytes
        always (= (read-byte stream) byte)))|#


(defun seek-to-app1 (stream)
  "Position STREAM after the #xFF, #xE1 marker in the JPEG stream
representing the APP1 segment. Raise an INVALID-EXIF-STREAM error if
no APP1 segment can be found."
  (do ((first-byte (read-byte stream nil) next-byte)
       (next-byte (read-byte stream nil) (read-byte stream nil)))
      ((not (and first-byte next-byte)))
    (cond ((and (= first-byte #xFF) (= next-byte #xE1))
           (return))
          ((and (= first-byte #xFF) (<= #xE0 next-byte #xEF))
           (let ((appn-size (read-jpeg-uint16 stream)))
             (file-position stream (+ (file-position stream)
                                      (- appn-size 2)))))
          ((= next-byte #xFF)
           ;; padding -- do nothing
           )
          (t
            (cl-user::afout 'out (format nil "invalid-exif-stream= ~A" stream))
        (error 'invalid-exif-stream)))))

;;mmm
(defun make-exif-from-stream (stream)
  "Extract an Exif object from the open (unsigned-byte 8) STREAM. The
stream must be positioned at the beginning of JPEG data. If the stream
is not a JPEG stream, raise INVALID-JPEG-STREAM. If the stream does
not contain Exif data, raise INVALID-EXIF-STREAM."
  (unwind-protect
      (ignore-errors
        (unless (check-bytes stream #xFF #xD8)
          (error 'invalid-jpeg-stream))
        ;;mmm
        ;;problem hangs here??
        (seek-to-app1 stream)
        (let ((size (read-jpeg-uint16 stream)))
          ;; ASCII "Exif"
          (unless (check-bytes stream #x45 #x78 #x69 #x66 #x00 #x00)
            (error 'invalid-exif-stream))
          (let ((data (make-array size :element-type '(unsigned-byte 8)))
                (offset (file-position stream)))
            (read-sequence data stream)
            ;; ASCII #\I or #\M
            (let ((endianness (if (= (aref data 0) (aref data 1) #x49)
                                  :lsb
                                :msb)))
              (destructuring-bind (get-32 get-16)
                  (if (eql endianness :lsb)
                      *reader-functions/lsb*
                    *reader-functions/msb*)
                (initialize-exif-ifds 
                 (make-instance 'exif
                                :file (ignore-errors (truename stream))
                                :data data
                                :offset offset
                                :endianness endianness
                                :get-32-function get-32
                                :get-16-function get-16))))))
        ;;end ignore-errors
        )
    (progn
      (cl-user::afout 'cl-user::out (format nil "ERROR In make-exif-from-stream"))
      NIL)
    ))

;;original version
#|
(defun make-exif-from-stream (stream)
  "Extract an Exif object from the open (unsigned-byte 8) STREAM. The
stream must be positioned at the beginning of JPEG data. If the stream
is not a JPEG stream, raise INVALID-JPEG-STREAM. If the stream does
not contain Exif data, raise INVALID-EXIF-STREAM."
  (unless (check-bytes stream #xFF #xD8)
    (error 'invalid-jpeg-stream))
  (seek-to-app1 stream)
  (let ((size (read-jpeg-uint16 stream)))
    ;; ASCII "Exif"
    (unless (check-bytes stream #x45 #x78 #x69 #x66 #x00 #x00)
      (error 'invalid-exif-stream))
    (let ((data (make-array size :element-type '(unsigned-byte 8)))
          (offset (file-position stream)))
      (read-sequence data stream)
      ;; ASCII #\I or #\M
      (let ((endianness (if (= (aref data 0) (aref data 1) #x49)
                            :lsb
                            :msb)))
        (destructuring-bind (get-32 get-16)
            (if (eql endianness :lsb)
                *reader-functions/lsb*
                *reader-functions/msb*)
          (initialize-exif-ifds 
           (make-instance 'exif
                          :file (ignore-errors (truename stream))
                          :data data
                          :offset offset
                          :endianness endianness
                          :get-32-function get-32
                          :get-16-function get-16)))))))|#


;;
(defun make-exif-from-file (file)
  (let ((exif)
        )
    (unwind-protect 
        (with-open-file (stream file
                                :direction :input
                                :element-type '(unsigned-byte 8))
          (cl-user::afout 'cl-user::out (format nil "in make-exif-from-file, stream= ~a" stream))
              (ignore-errors
                (setf exif  (make-exif-from-stream stream)))
              
          #| didn't help, unnecessary
             (unwind-protect
              (ignore-errors
                (setf exif  (make-exif-from-stream stream)))
            (progn 
            (close stream) ;;not needed with (with-open-file, LWCLdocs
             (cl-user::afout 'cl-user::out 
                             (format nil "After close,  stream= ~A"stream))|#
            
      (unless exif 
          (cl-user::afout 'cl-user::out 
                          (format nil "~&init file failed to load.~% stream= ~A"stream)))
      ;;end with-open-file
          exif))))
;;TEST
;;  (zpb-exif::make-exif-from-file  (pathname "C:\\Temp\\apple farm.jpg"))

#|(defun make-exif-from-file (file)
  (let ((exif)
        )
    (IGNORE-ERRORS ;if this fails, don't enter debugger
      (with-open-file (stream file
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (setf exif (make-exif-from-stream stream)))
    (unless exif (cl-user::afout 'out (format nil "~&Init file failed to load.~%")))
    exif)))|#
;;mmm
;; original version -- caused errors when it couldn't read or find exif stream
#|
(defun make-exif-from-file (file)
  (with-open-file (stream file
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (make-exif-from-stream stream))) 
|#
;;TRY IGNORE-ERRORS and/or UNWIND-PROTECT to shied from exif stream errors which
;; are stopping the program
;; example from LW CL symbols
#|
 (defun load-init-file (program)
   (let ((win nil))
     (IGNORE-ERRORS ;if this fails, don't enter debugger
       (load (merge-pathnames (make-pathname :name program :type :lisp)
                              (user-homedir-pathname)))
       (setq win t))
     (unless win (format t "~&Init file failed to load.~%"))
     win))
 (load-init-file "no-such-program")
>>  Init file failed to load.
NIL
|#
(defun cl-user::testex (fileNum)
  (setf cl-user::out nil)
  (let
      ((exif)
       (file)
       )
    (if (= fileNum 1)
        (setf file "C:\\TOM\\LISP PROJECTS TS\\screensaver\\IMG_6779.jpg")
      (setf file  "C:\\TOM\\LISP PROJECTS TS\\screensaver\\OKC Early5.jpg"))
  (setf exif (zpb-exif::make-exif file))
  (cl-user::afout 'cl-user::out (format nil "exif= ~A" exif))))

(defun make-exif (object)
  "Read and create an exif object from OBJECT, which may be a pathname
designator or a stream."
  (let ((exif)
        )
    (cl-user::afout 'cl-user::out (format nil "~&make-exif-from-stream object= ~A~%" object))
    (unwind-protect
    (IGNORE-ERRORS 
      (typecase object                                   ;;WAS etypecase object
        ((or string pathname) (setf exif (make-exif-from-file object)))
        (stream (setf exif (make-exif-from-stream object)))))
      (progn (unless exif (cl-user::afout 'cl-user::out (format nil "~&Init file failed to load for object= ~A~%" object)))
      exif))))

;;ETYPECASE DESCRIPTION: Each of the normal-clauses is then considered in turn. If the test-key is of the type given by the clauses's type, the forms in that clause are evaluated as an implicit progn, and the values it returns are returned as the value of the typecase, ctypecase, or etypecase form. ETYPCASE CAUSES NON-RETURNABLE ERROR.


#|
;;was
(defun make-exif (object)
  "Read and create an exif object from OBJECT, which may be a pathname
designator or a stream."
  (etypecase object
    ((or string pathname) (make-exif-from-file object))
    (stream (make-exif-from-stream object))))
|#






;;; Tagsets
;;;
;;; This is necessary because, unfortunately, tags are only unique
;;; within a particular IFD. The GPS and interoperability IFDs, for
;;; example, have conflicting tags.

(defclass tagset-entry ()
  ((tag
    :initarg :tag
    :reader tag)
   (name
    :initarg :name
    :reader name)
   (type
    :initarg :type
    :reader type)))

(defun tag-name (code tagset)
  (let ((table (get tagset 'tagset-code-table)))
    (if table
        (let ((entry (gethash code table)))
          (when entry
            (name entry)))
        (error "~A is not a known tagset" tagset))))

(defun tag-type (code tagset)
  (let ((table (get tagset 'tagset-code-table)))
    (if table
        (let ((entry (gethash code table)))
          (when entry
            (type entry)))
        (error "~A is not a known tagset" tagset))))

(defun tag-code (name tagset)
  (let ((table (get tagset 'tagset-name-table)))
    (if table
        (nth-value 0 (gethash name table))
        (error "~A is not a known tagset" tagset))))

(defmacro define-tagset (name &body tag-definitions)
  (let ((name-table (gensym))
        (code-table (gensym))
        (setfs '()))
    `(let ((,name-table (make-hash-table :test 'equalp))
           (,code-table (make-hash-table)))
      (setf (get ',name 'tagset-name-table) ,name-table
       (get ',name 'tagset-code-table) ,code-table)
      ,@(dolist (definition tag-definitions setfs)
          (destructuring-bind (&key tag name type)
              definition
            (push `(setf (gethash ,name ,name-table) ,tag
                    (gethash ,tag ,code-table) (make-instance 'tagset-entry
                                                :tag ,tag
                                                :name ,name
                                                :type ',type))
                    setfs))))))


;;; Converting some Exif values to somewhat more parsed values

(defun exif-type-parser-fun (exif-type)
  (or (get exif-type 'exif-type-parser-fun) 'identity))

(defmacro define-exif-type (name (base-type) &rest options)
  (declare (ignore base-type))
  (let ((forms
         (loop for ((type . parameters)) on options
               when (eql type :parser)
               collect (destructuring-bind (lambda-list &body body)
                           parameters
                         `(setf (get ',name 'exif-type-parser-fun)
                           (lambda ,lambda-list ,@body))))))
    (when forms
      `(progn ,@forms))))

(defun parse-datetime (string)
  "Convert an Exif datetime string in the form \"YYYY:MM:DD hh:mm:ss\"
to a universal time."
  ;; YYYY:MM:DD hh:mm:ss
  ;; 0123456789012345678
  (flet ((integer-at (start end)
           (parse-integer string :start start :end end)))
    (encode-universal-time (integer-at 17 19)
                           (integer-at 14 16)
                           (integer-at 11 13)
                           (integer-at  8 10)
                           (integer-at  5  7)
                           (integer-at  0  4))))

;;;
;;; It was no fun to type all these in from JEITA CP-3451
;;;

;;; Generic types, used multiple times

(define-exif-type datetime (ascii)
  (:parser (value)
    (parse-datetime value)))

(define-exif-type floatable-rational (rational)
  (:parser (value)
    (float value)))

;;; One-off types; used for a specific tag in a tagset

(define-exif-type orientation (short)
  (:parser (value)
    (case value
      (1 :normal)
      (2 :flipped-horizontally)
      (3 :rotated-180)
      (4 :flipped-vertically)
      (5 :rotated-270-and-flipped-horizontally)
      (6 :rotated-270)
      (7 :rotated-90-and-flipped-vertically)
      (8 :rotated-90)
      (otherwise :reserved))))

(define-exif-type exposure-program (short)
  (:parser (value)
    (case value
      (0 :not-defined)
      (1 :manual)
      (2 :normal-program)
      (3 :aperture-priority)
      (4 :shutter-priority)
      (5 :creative-program)
      (6 :action-program)
      (7 :portrait-mode)
      (8 :landscape-mode)
      (otherwise :reserved))))

(define-exif-type sensing-method (short)
  (:parser (value)
    (case value
      (1 :not-defined)
      (2 :one-chip-color-area-sensor)
      (3 :two-chip-color-area-sensor)
      (4 :three-chip-color-area-sensor)
      (5 :color-sequential-area-sensor)
      (6 :trilinear-sensor)
      (7 :color-sequential-linear-sensor)
      (otherwise :reserved))))

(define-exif-type metering-mode (short)
  (:parser (value)
    (case value
      (0 :unknown)
      (1 :average)
      (2 :center-weighted-average)
      (3 :spot)
      (4 :multispot)
      (5 :pattern)
      (6 :partial)
      (255 :other)
      (otherwise :reserved))))

(define-exif-type light-source (short)
  (:parser (value)
    (case value
      (0 :unknown)
      (1 :daylight)
      (2 :flourescent)
      (3 :tungsten)
      (4 :flash)
      (9 :fine-weather)
      (10 :cloudy-weather)
      (11 :shade)
      (12 :daylight-flourescent)
      (13 :day-white-flourescent)
      (14 :cool-white-flourescent)
      (15 :white-flourescent)
      (17 :standard-light-a)
      (18 :standard-light-b)
      (19 :standard-light-c)
      (20 :d55)
      (21 :d65)
      (22 :d75)
      (23 :d50)
      (24 :iso-studio-tungsten)
      (255 :other-light-source)
      (otherwise :reserved))))
  
(define-exif-type flash (short)
  (:parser (value)
    ;; FIXME: a gross simplification
    (logbitp 0 value)))

(define-exif-type exposure-mode (short)
  (:parser (value)
    (case value
      (0 :auto-exposure)
      (1 :manual-exposure)
      (2 :auto-bracket)
      (otherwise :reserved))))

(define-exif-type white-balance (short)
  (:parser (value)
    (case value
      (0 :auto)
      (1 :manual)
      (otherwise :reserved))))

(define-exif-type scene-capture-type (short)
  (:parser (value)
    (case value
      (0 :standard)
      (1 :landscape)
      (2 :portrait)
      (3 :night-scene)
      (otherwise :reserved))))

(define-exif-type gain-control (short)
  (:parser (value)
    (case value
      (0 :none)
      (1 :low-gain-up)
      (2 :high-gain-up)
      (3 :low-gain-down)
      (4 :high-gain-down)
      (otherwise :reserved))))

(define-exif-type contrast (short)
  (:parser (value)
    (case value
      (0 :normal)
      (1 :soft)
      (2 :hard)
      (otherwise :reserved))))

(define-exif-type saturation (short)
  (:parser (value)
    (case value
      (0 :normal)
      (1 :low-saturation)
      (2 :high-saturation)
      (otherwise :reserved))))

(define-exif-type sharpness (short)
  (:parser (value)
    (case value
      (0 :normal)
      (1 :soft)
      (2 :hard)
      (otherwise :reserved))))

(define-exif-type subject-distance-range (short)
  (:parser (value)
    (case value
      (0 :unknown)
      (1 :macro)
      (2 :close-view)
      (3 :distant-view)
      (otherwise :reserved))))

(define-exif-type exif-version (unknown)
  (:parser (value)
    ;; FIXME: ASCII-centric
    (let ((version-string (make-array 5 :initial-element (char-code #\.))))
      (replace version-string value :start2 0 :end2 2)
      (replace version-string value :start1 3 :start2 2)
      (string-trim '(#\0) (map 'string #'code-char version-string)))))

(define-exif-type color-space (short)
  (:parser (value)
    (case value
      (1 :srgb)
      (#xFFFF :uncalibrated)
      (otherwise :reserved))))

(define-exif-type user-comment (undefined)
  (:parser (value)
    (flet ((starts-with (prefix)
             (loop for i across prefix
                   for j across value
                   always (= i j))))
      (cond ((or (starts-with #(0 0 0 0 0 0 0 0))
                 (starts-with #(#x41 #x53 #x43 #x49 #x49)))
             ;; ASCII encoding
             (let ((first-null (or (position 0 value :start 8)
                                   (length value))))
               (map 'string #'code-char (subseq value 8 first-null))))
            (t value)))))

(define-exif-type ycbcr-positioning (short)
  (:parser (value)
    (case value
      (1 :centered)
      (2 :co-sited)
      (otherwise :reserved))))

(define-tagset image
  ;; Tags relating to image data structure
  (:tag #x0100 :name "ImageWidth" :type short/long)
  (:tag #x0101 :name "ImageHeight" :type short/long)
  (:tag #x0102 :name "BitsPerSample" :type short)
  (:tag #x0103 :name "Compression" :type short)
  (:tag #x0106 :name "PhotometricInterpretation" :type short)
  (:tag #x0112 :name "Orientation" :type orientation)
  (:tag #x0115 :name "SamplesPerPixel" :type short)
  (:tag #x011C :name "PlanarConfiguration" :type short)
  (:tag #x0212 :name "YCbCrSubSampling" :type short)
  (:tag #x0213 :name "YCbCrPositioning" :type ycbcr-positioning)
  (:tag #x011A :name "XResolution" :type rational)
  (:tag #x011B :name "YResolution" :type rational)
  (:tag #x0128 :name "ResolutionUnit" :type short)
  ;; Tags relating to recording offset
  (:tag #x0111 :name "StripOffsets" :type short/long)
  (:tag #x0116 :name "RowsPerStrip" :type short/long)
  (:tag #x0117 :name "StripByteCounts" :type short/long)
  (:tag #x0201 :name "JPEGInterchangeFormat" :type long)
  (:tag #x0202 :name "JPEGInterchangeFormatLength" :type long)
  ;; Tags relating to image data characteristics
  (:tag #x012D :name "TransferFunction" :type short)
  (:tag #x013E :name "WhitePoint" :type rational)
  (:tag #x013F :name "PrimaryChromaticities" :type rational)
  (:tag #x0211 :name "YCbCrCoefficients" :type rational)
  (:tag #x0214 :name "ReferenceBlackWhite" :type rational)
  ;; Other tags
  (:tag #x0132 :name "DateTime" :type datetime)
  (:tag #x010E :name "ImageDescription" :type ascii)
  (:tag #x010F :name "Make" :type ascii)
  (:tag #x0110 :name "Model" :type ascii)
  (:tag #x0131 :name "Software" :type ascii)
  (:tag #x013B :name "Artist" :type ascii)
  (:tag #x8298 :name "Copyright" :type ascii))

(define-tagset exif
  ;; Tags Relating to Version
  (:tag #x9000 :name "ExifVersion" :type exif-version)
  (:tag #xA000 :name "FlashpixVersion" :type undefined)
  ;; Tag Relating to Image Data Characteristics
  (:tag #xA001 :name "ColorSpace" :type color-space)
  (:tag #xA500 :name "Gamma" :type floatable-rational)
  ;; Tags Relating to Image Configuration
  (:tag #x9101 :name "ComponentsConfiguration" :type undefined)
  (:tag #x9102 :name "CompressedBitsPerPixel" :type rational)
  (:tag #xA002 :name "PixelXDimension" :type short/long)
  (:tag #xA003 :name "PixelYDimension" :type short/long)
  ;; Tags Relating to User Information
  (:tag #x927C :name "MakerNote" :type undefined)
  (:tag #x9286 :name "UserComment" :type user-comment)
  ;; Tag Relating to Related File Information
  (:tag #xA004 :name "RelatedSoundFile" :type ascii)
  ;; Tags Relating to Date and Time
  (:tag #x9003 :name "DateTimeOriginal" :type datetime)
  (:tag #x9004 :name "DateTimeDigitized" :type datetime)
  (:tag #x9290 :name "SubSecTime" :type ascii)
  (:tag #x9291 :name "SubSecTimeOriginal" :type ascii)
  (:tag #x9292 :name "SubSecTimeDigitized" :type ascii)
  ;; Tags Relating to Picture-Taking Conditions
  (:tag #x829A :name "ExposureTime" :type floatable-rational)
  (:tag #x829D :name "FNumber" :type floatable-rational)
  (:tag #x8822 :name "ExposureProgram" :type exposure-program)
  (:tag #x8824 :name "SpectralSensitivity" :type ascii)
  (:tag #x8827 :name "ISOSpeedRatings" :type short)
  (:tag #x8828 :name "OECF" :type undefined)
  (:tag #x9201 :name "ShutterSpeedValue" :type floatable-rational)
  (:tag #x9202 :name "ApertureValue" :type floatable-rational)
  (:tag #x9203 :name "BrightnessValue" :type srational)
  (:tag #x9204 :name "ExposureBiasValue" :type srational)
  (:tag #x9205 :name "MaxApertureValue" :type floatable-rational)
  (:tag #x9206 :name "SubjectDistance" :type floatable-rational)
  (:tag #x9207 :name "MeteringMode" :type metering-mode)
  (:tag #x9208 :name "LightSource" :type light-source)
  (:tag #x9209 :name "Flash" :type flash)
  (:tag #x920A :name "FocalLength" :type floatable-rational)
  (:tag #x9214 :name "SubjectArea" :type short)
  (:tag #xA20B :name "FlashEngergy" :type rational)
  (:tag #xA20C :name "SpatialFrquencyResponse" :type undefined)
  (:tag #xA20E :name "FocalPlaneXResolution" :type floatable-rational)
  (:tag #xA20F :name "FocalPlaneYResolution" :type floatable-rational)
  (:tag #xA210 :name "FocalPlaneResolutionUnit" :type short)
  (:tag #xA214 :name "SubjectLocation" :type short)
  (:tag #xA215 :name "ExposureIndex" :type rational)
  (:tag #xA217 :name "SensingMethod" :type sensing-method)
  (:tag #xA300 :name "FileSource" :type undefined)
  (:tag #xA301 :name "SceneType" :type undefined)
  (:tag #xA302 :name "CFAPattern" :type undefined)
  (:tag #xA401 :name "CustomRendered" :type short)
  (:tag #xA402 :name "ExposureMode" :type exposure-mode)
  (:tag #xA403 :name "WhiteBalance" :type white-balance)
  (:tag #xA404 :name "DigitalZoomRatio" :type rational)
  (:tag #xA405 :name "FocalLengthIn35mmFilm" :type short)
  (:tag #xA406 :name "SceneCaptureType" :type scene-capture-type)
  (:tag #xA407 :name "GainControl" :type rational)
  (:tag #xA408 :name "Contrast" :type contrast)
  (:tag #xA409 :name "Saturation" :type saturation)
  (:tag #xA40A :name "Sharpness" :type sharpness)
  (:tag #xA40B :name "DeviceSettingDescription" :type undefined)
  (:tag #xA40C :name "SubjectDistanceRange" :type subject-distance-range)
  ;; Other Tags
  (:tag #xA420 :name "ImageUniqueID" :type ascii))

(define-tagset gps
  ;; Tags Relating to GPS
  (:tag #x0000 :name "GPSVersionID" :type byte)
  (:tag #x0001 :name "GPSLatitudeRef" :type ascii)
  (:tag #x0002 :name "GPSLatitude" :type rational)
  (:tag #x0003 :name "GPSLongitudeRef" :type ascii)
  (:tag #x0004 :name "GPSLongitude" :type ascii)
  (:tag #x0005 :name "GPSAltitudeRef" :type byte)
  (:tag #x0006 :name "GPSAltitude" :type rational)
  (:tag #x0007 :name "GPSTimeStamp" :type rational)
  (:tag #x0008 :name "GPSSatellites" :type ascii)
  (:tag #x0009 :name "GPSStatus" :type ascii)
  (:tag #x000A :name "GPSMeasureMode" :type ascii)
  (:tag #x000B :name "GPSDOP" :type rational)
  (:tag #x000C :name "GPSSpeedRef" :type ascii)
  (:tag #x000D :name "GPSSpeed" :type rational)
  (:tag #x000E :name "GPSTrackRef" :type ascii)
  (:tag #x000F :name "GPSTrack" :type rational)
  (:tag #x0010 :name "GPSImgDirectionRef" :type ascii)
  (:tag #x0011 :name "GPSImgDirection" :type rational)
  (:tag #x0012 :name "GPSMapDatum" :type ascii)
  (:tag #x0013 :name "GPSDestLatitudeRef" :type ascii)
  (:tag #x0014 :name "GPSDestLatitude" :type rational)
  (:tag #x0015 :name "GPSDestLongitudeRef" :type ascii)
  (:tag #x0016 :name "GPSDestLongitude" :type rational)
  (:tag #x0017 :name "GPSDestBearingRef" :type ascii)
  (:tag #x0018 :name "GPSDestBearing" :type rational)
  (:tag #x0019 :name "GPSDestDistanceRef" :type ascii)
  (:tag #x001A :name "GPSDestDistance" :type rational)
  (:tag #x001B :name "GPSProcessingMethod" :type undefined)
  (:tag #x001C :name "GPSAreaInformation" :type undefined)
  (:tag #x001D :name "GPSDateStamp" :type ascii)
  (:tag #x001E :name "GPSDifferential" :type short))

(define-tagset interoperability
  (:tag #x0001 :name "InteroperabilityIndex" :type ascii))

(defun tag-designator (tag tagset)
  (etypecase tag
    (string (tag-code tag tagset))
    (symbol (tag-code (symbol-name tag) tagset))
    (number tag)))

(defun ifd-entry (tag ifd)
  (unless (null ifd) ;; I added
  (when ifd
    (let ((tag (tag-designator tag (tagset ifd))))
      (when tag
        (bisect-find tag (entries ifd) :key #'tag))))))

(defun find-ifd-entry (tag exif)
  (unless (null exif)   ;;I added
  (or (ifd-entry tag (image-ifd exif))
      (ifd-entry tag (exif-ifd exif))
      (ifd-entry tag (gps-ifd exif)))))

(defun exif-value (tag exif)
  (unless (null exif)
  (entry-value (find-ifd-entry tag exif))))

(defun parsed-entry-value (ifd-entry)
  (when ifd-entry
    (let* ((type (tag-type (tag ifd-entry) (tagset (ifd ifd-entry))))
           (fun (exif-type-parser-fun type)))
      (funcall fun (entry-value ifd-entry)))))
    
(defun parsed-exif-value (tag exif)
  (let ((entry (find-ifd-entry tag exif)))
    (when entry
      (parsed-entry-value entry))))


(defmethod print-object ((ifd-entry ifd-entry) stream)
  (print-unreadable-object (ifd-entry stream :type t :identity t)
    (let ((name (or (tag-name (tag ifd-entry) (tagset (ifd ifd-entry)))
                    "(Unknown Tag)")))
      (format stream "~S #x~X/~D/~D"
              name
              (tag ifd-entry)
              (type ifd-entry)
              (count ifd-entry)))))


;;; Miscellaneous utility functions

(defun thumbnail-image (exif)
  "Return an (UNSIGNED-BYTE 8) vector containing the JPEG data for the
thumbnail image in EXIF, if present. If there is no thumbnail, return
NIL."
  (let ((ifd (thumbnail-ifd exif)))
    (flet ((val (tag)
             (entry-value (ifd-entry tag ifd))))
      (when ifd
        (let ((offset (val "JPEGInterchangeFormat"))
              (length (val "JPEGInterchangeFormatLength")))
          (when (and offset length)
            (let ((data (make-array length
                                    :element-type '(unsigned-byte 8))))
              (replace data (exif-data exif)
                       :start2 offset)
              data)))))))

;;; Similar to exifinfo.cl's parse-exif-data

(defun parse-exif-data (file)
  (let ((exif (make-exif file)))
    (cond  ;;I added
           ((null exif) nil)
           (t            
            (labels ((val (tag)
                       (exif-value tag exif))
                     (float-val (tag)
                       (let ((value (val tag)))
                         (and value (float value)))))
              (let* ((flash-value (val "Flash"))
                     (flash-fired-p (and flash-value (not (logand flash-value 1)))))
                (list :file (file exif)
                      :make (val "Make")
                      :model (val "Model")
                      :date (val "DateTime")
                      :comment nil              ; This isn't actually useful
                      :orientation (val "Orientation")
                      :exposure (float-val "ExposureTime")
                      :f-number (float-val "FNumber")
                      :iso-rating (val "IsoSpeedRatings")
                      :exposure-bias-value (float-val "ExposureBiasValue")
                      :subject-distance (float-val "SubjectDistance")
                      :flash flash-fired-p
                      :focal-length (float-val "FocalLength")
                      :image-width (val "PixelXDimension")
                      :image-length (val "PixelYDimension"))))))
    ))


(defun ifd-alist (ifd &key parsedp)
  "Return all the values in IFD as an alist."
  (when ifd
    (loop for entry across (entries ifd)
          for name = (or (tag-name (tag entry) (tagset ifd))
                         (format nil "Unknown Tag #x~4,'0X" (tag entry)))
          collect (cons name
                        (if parsedp
                            (parsed-entry-value entry)
                            (entry-value entry))))))

(defun exif-alist (exif &key parsedp)
  "Return the keys and values from the Image, Exif, and GPS IFDs of EXIF."
  (with-slots (image-ifd exif-ifd gps-ifd)
      exif
    (nconc (ifd-alist image-ifd :parsedp parsedp)
           (ifd-alist exif-ifd :parsedp parsedp)
           (ifd-alist gps-ifd :parsedp parsedp))))



;; ----------------------- MY EXAMPLE ---------------------
;;
;;sss
;;zpb-exif: package 
;;
#|(defparameter *exif* (zpb-exif:make-exif #p"C:\\TOM\\LISP PROJECTS TS\\screensaver\\test-photo1.jpg"))                       ;;#p"gg_gps.JPG"))

;;=> *EXIF*

 (zpb-exif:EXIF-VALUE "FNumber" zpb-exif::*exif*)
;;=> 9/2

 (zpb-exif:parsed-exif-value "MeteringMode" *exif*)
;;=> :PATTERN

;;xxx
 (zpb-exif:exif-value :DateTimeOriginal *exif*)
;;=> "2003:11:23 18:07:37"

 (zpb-exif:exif-value #x0112 *exif*)   ; #x0112 => orientation tag in the image IFD
;;=> 6

 (zpb-exif:parsed-exif-value #x0112 *exif*)
;;=> :ROTATED-270

 (zpb-exif:exif-value :ISOSpeedRating *exif*)   ; Field not present in this particular image
;;=> NIL

 (list (zpb-exif:exif-value "GPSLatitude" *exif*)
        (zpb-exif:exif-value "GPSLatitudeRef" *exif*)
        (zpb-exif:exif-value "GPSLongitude" *exif*)
        (zpb-exif:exif-value "GPSLongitudeRef" *exif*))
;;=> (#(39 54 56) "N" #(116 23 27) "E")
|#


;;------------------------- HHH SEE H-Conditions.lisp -------------------------
;; has examples of how to unwind-protect and ignore-errors and use of conditions, restart, etc.

