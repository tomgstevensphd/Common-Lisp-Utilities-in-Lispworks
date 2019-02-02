;;*************************** C-characters.lisp *************************
;;
;;
#|
2.1.3 Standard Characters

All implementations must support a character repertoire called standard-char; characters that are members of that repertoire are called standard characters.

The standard-char repertoire consists of the non-graphic character newline, the graphic character space, and the following additional ninety-four graphic characters or their equivalents:

Graphic ID  Glyph  Description  Graphic ID  Glyph  Description  
LA01        a      small a      LN01        n      small n      
LA02        A      capital A    LN02        N      capital N    
LB01        b      small b      LO01        o      small o      
LB02        B      capital B    LO02        O      capital O    
LC01        c      small c      LP01        p      small p      
LC02        C      capital C    LP02        P      capital P    
LD01        d      small d      LQ01        q      small q      
LD02        D      capital D    LQ02        Q      capital Q    
LE01        e      small e      LR01        r      small r      
LE02        E      capital E    LR02        R      capital R    
LF01        f      small f      LS01        s      small s      
LF02        F      capital F    LS02        S      capital S    
LG01        g      small g      LT01        t      small t      
LG02        G      capital G    LT02        T      capital T    
LH01        h      small h      LU01        u      small u      
LH02        H      capital H    LU02        U      capital U    
LI01        i      small i      LV01        v      small v      
LI02        I      capital I    LV02        V      capital V    
LJ01        j      small j      LW01        w      small w      
LJ02        J      capital J    LW02        W      capital W    
LK01        k      small k      LX01        x      small x      
LK02        K      capital K    LX02        X      capital X    
LL01        l      small l      LY01        y      small y      
LL02        L      capital L    LY02        Y      capital Y    
LM01        m      small m      LZ01        z      small z      
LM02        M      capital M    LZ02        Z      capital Z    
Figure 2-3. Standard Character Subrepertoire (Part 1 of 3: Latin Characters)

Graphic ID  Glyph  Description  Graphic ID  Glyph  Description  
ND01        1      digit 1      ND06        6      digit 6      
ND02        2      digit 2      ND07        7      digit 7      
ND03        3      digit 3      ND08        8      digit 8      
ND04        4      digit 4      ND09        9      digit 9      
ND05        5      digit 5      ND10        0      digit 0      
Figure 2-4. Standard Character Subrepertoire (Part 2 of 3: Numeric Characters)


Graphic ID  Glyph  Description                              
SP02        !      exclamation mark                         
SC03        $      dollar sign                              
SP04        "      quotation mark, or double quote          
SP05        '      apostrophe, or [single] quote            
SP06        (      left parenthesis, or open parenthesis    
SP07        )      right parenthesis, or close parenthesis  
SP08        ,      comma                                    
SP09        _      low line, or underscore                  
SP10        -      hyphen, or minus [sign]                  
SP11        .      full stop, period, or dot                
SP12        /      solidus, or slash                        
SP13        :      colon                                    
SP14        ;      semicolon                                
SP15        ?      question mark                            
SA01        +      plus [sign]                              
SA03        <      less-than [sign]                         
SA04        =      equals [sign]                            
SA05        >      greater-than [sign]                      
SM01        #      number sign, or sharp[sign]              
SM02        %      percent [sign]                           
SM03        &      ampersand                                
SM04        *      asterisk, or star                        
SM05        @      commercial at, or at-sign                
SM06        [      left [square] bracket                    
SM07        \      reverse solidus, or backslash            
SM08        ]      right [square] bracket                   
SM11        {      left curly bracket, or left brace        
SM13        |      vertical bar                             
SM14        }      right curly bracket, or right brace      
SD13        `      grave accent, or backquote               
SD15        ^      circumflex accent                        
SD19        ~      tilde                                    
Figure 2-5. Standard Character Subrepertoire (Part 3 of 3: Special Characters)

The graphic IDs are not used within Common Lisp, but are provided for cross reference purposes with ISO 6937/2. Note that the first letter of the graphic ID categorizes the character as follows: L---Latin, N---Numeric, S---Special.

;;xxx-----------------------------------------------------------------------------------------

[LISPWORKS][Common Lisp HyperSpec (TM)] [Previous][Up][Next]

13.2 The Characters Dictionary

System Class CHARACTER

Type BASE-CHAR

Type STANDARD-CHAR

Type EXTENDED-CHAR

Function CHAR=, CHAR/=, CHAR<, CHAR>, CHAR<=, CHAR>=, CHAR-EQUAL, CHAR-NOT-EQUAL, CHAR-LESSP, CHAR-GREATERP, CHAR-NOT-GREATERP, CHAR-NOT-LESSP

Function CHARACTER

Function CHARACTERP

Function ALPHA-CHAR-P

Function ALPHANUMERICP

Function DIGIT-CHAR

Function DIGIT-CHAR-P

Function GRAPHIC-CHAR-P

Function STANDARD-CHAR-P

Function CHAR-UPCASE, CHAR-DOWNCASE

Function UPPER-CASE-P, LOWER-CASE-P, BOTH-CASE-P

Function CHAR-CODE

Function CHAR-INT

Function CODE-CHAR

Constant Variable CHAR-CODE-LIMIT

Function CHAR-NAME

Function NAME-CHAR

[Starting Points][Contents][Index][Symbols][Glossary][Issues]
Copyright 1996-2005, LispWorks Ltd. All rights reserved.
;;xxx -----------------------------------------------------------------------------------------


Function CODE-CHAR-------------------------
Syntax:

code-char code => char-p

Arguments and Values:

code---a character code.

char-p---a character or nil.

Description:

Returns a character with the code attribute given by code. If no such character exists and one cannot be created, nil is returned.

Examples:

(code-char 65.) =>  #\A  ;in an implementation using ASCII codes
(code-char (char-code #\Space)) =>  #\Space  ;in any implementation

;;xxx --------------------------------------------------------------

Function CHAR-NAME
Syntax:

char-name character => name

Arguments and Values:

character---a character.

name---a string or nil.

Description:

Returns a string that is the name of the character, or nil if the character has no name.

All non-graphic characters are required to have names unless they have some implementation-defined attribute which is not null. Whether or not other characters have names is implementation-dependent.

The standard characters <Newline> and <Space> have the respective names "Newline" and "Space". The semi-standard characters <Tab>, <Page>, <Rubout>, <Linefeed>, <Return>, and <Backspace> (if they are supported by the implementation) have the respective names "Tab", "Page", "Rubout", "Linefeed", "Return", and "Backspace" (in the indicated case, even though name lookup by ``#\'' and by the function name-char is not case sensitive).

Examples:

 (char-name #\ ) =>  "Space"
 (char-name #\Space) =>  "Space"
 (char-name #\Page) =>  "Page"

 (char-name #\a)
=>  NIL
OR=>  "LOWERCASE-a"
OR=>  "Small-A"
OR=>  "LA01"

 (char-name #\A)
=>  NIL
OR=>  "UPPERCASE-A"
OR=>  "Capital-A"
OR=>  "LA02"

 ;; Even though its CHAR-NAME can vary, #\A prints as #\A
 (prin1-to-string (read-from-string (format nil "#\\~A" (or (char-name #\A) "A"))))
=>  "#\\A"

;;xxx --------------------------------------------------

Function NAME-CHAR
Syntax:

name-char name => char-p

Arguments and Values:

name---a string designator.

char-p---a character or nil.

Description:

Returns the character object whose name is name (as determined by string-equal---i.e., lookup is not case sensitive). If such a character does not exist, nil is returned.

Examples:

(name-char 'space) =>  #\Space
(name-char "space") =>  #\Space
(name-char "Space") =>  #\Space
(let ((x (char-name #\a)))
  (or (not x) (eql (name-char x) #\a))) =>  true
Affected By: None.

Exceptional Situations:

Should signal an error of type type-error if name is not a string designator.
|#