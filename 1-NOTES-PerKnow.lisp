;;************************** 1-NOTES-PerKnow.lisp ************************
;;
#|
Whether to use CLOS or my own list-based knowledge rep system for at least the dictionary meanings.  Must? be able to change super and sub classes on the fly. These would be links to super and sub classes of the actual words.  (or just make them members of slot-lists?)
CAN DO?--USE add-direct-subclass to the superclass to do this (must know the names of both sub and superclass).
NOTE: Can use (setf slot-value-using-class) new-value class object slot. These methods implement the full behavior of this generic function for slots with allocation :instance and :class.

Probably set the word to my dictionary definiton structure, whatever it is.
(Problem of word variations--plural, tense, etc.)
Synonyms, antanyms, etc in slots?

Defining sentences would have to be decomposable also.

Be consistent with Anderson's ACT propositional (S-R) ideas.

|#


