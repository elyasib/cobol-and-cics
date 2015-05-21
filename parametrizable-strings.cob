      *    THESE PARAGRAPHS ALLOW YOU TO USE PARAMETRIZABLE STRINGS
      *    REPLACING THE PARAMETERS WITH WHICHEVER VALUES ARE RELEVANT 
      *    TO EACH STRING. THIS COULD BE USED FOR EXAMPLE, TO BUILD 
      *    EMAILS OR SMSs FROM A COBOL BACK-END, PREVENTING THE LEAK OF 
      *    SENTITIVE INFORMATION.
      *    ALL VARIABLES AND PARAGRAPHS NEEDED TO IMPLEMENT THE 
      *    FUNCTIONALITY ARE INCLUDED BELOW. SOME VALUES (eg. LEGTHs,
      *    DBM functions) MAY NEED TO BE ADJUSTED.


      *=================================================================
      *                  WORKING-STORAGE SECTION                        
      *=================================================================
       WORKING-STORAGE SECTION.

       01  WS-VARCHARS.
           05  WS-STRING1.
               49  WS-STRING1-L             PIC S9(4) COMP.
               49  WS-STRING1-T             PIC X(400).
           05  WS-STRING2.
               49  WS-STRING2-L             PIC S9(4) COMP.
               49  WS-STRING2-T             PIC X(400).
           05  WS-STRING3.
               49  WS-STRING3-L             PIC S9(4) COMP.
               49  WS-STRING3-T             PIC X(400).
        
       01  SW-SEARCH-PATTERN                PIC 9(01)   VALUE 0.
           88  SW-SEARCH-Y                              VALUE 1.
           88  SW-SEARCH-N                              VALUE 0.
           
       77  WS-MAX-INDX                      PIC 9(02) VALUE 0.
       77  WS-MIN-INDX                      PIC 9(02) VALUE 0.
       77  ROW-IDENTIFIER                   PIC X(50) VALUE 
                                                'whatever value it has'.
                                                
       01  WS-REPLACING-VARS.
           05  WS-STR.
               49  WS-STR-L                 PIC S9(4) COMP.
               49  WS-STR-T                 PIC X(400).
           05  WS-SUBTITUTION-TBL.
               10  WS-SUB                   OCCURS 100 TIMES.
                   49  WS-SUB-IX            PIc X(3).
                   49  WS-SUB-L             PIC S9(4) COMP.
                   49  WS-SUB-T             PIC X(50). 
           05  WS-TEMP                      PIC X(400).      
           05  WS-TLLY-CTR                  PIC S9(4) COMP.
...


      *=================================================================
      *    GET-EDITABLE-MSG. Gets substrings of a varchar field that has
      *    special sections of variable lenght. Each section may have 
      *    "editable" words denoted by @ and a numeric index, eg. @00, 
      *    @03, @78, etc.
      *=================================================================
       2000-GET-EDITABLE-MSG.
          
           EXEC SQL
               SELECT substring(field_name,
                       patindex('%<a>%',field_name)+3,
                       patindex('%</a>%',field_name)
                       -patindex('%<a>%',field_name)-3),
                      substring(field_name,
                       patindex('%<b>%',field_name)+3,
                       patindex('%</b>%',field_name)
                       -patindex('%<b>%',field_name)-3),
                      substring(field_name,
                       patindex('%<b>%',field_name)+3,
                       patindex('%</b>%',field_name)
                       -patindex('%<b>%',field_name)-3)
                 INTO  :WS-STRING1
                      ,:WS-STRING2
                      ,:WS-STRING3
                 FROM  table_name with(nolock)
                WHERE  row_identifier = :ROW-IDENTIFIER
           END-EXEC

           MOVE SQLCODE

           EVALUATE SQLCODE
             WHEN 0 AND (WS-STRING1-L = 0 OR WS-STRING2-L = 0
             WS-STRING3-L = 0)
                 MOVE    5                  TO OUT-ERR-COD
                 MOVE 'STRNGS INCOMPLETE'   TO OUT-ERR-MSG
                 PERFORM 3000-END
             WHEN 0
                 CONTINUE
             WHEN 100
                 MOVE    6                  TO OUT-ERR-COD
                 MOVE 'STRNGS NOT FOUND'    TO OUT-ERR-MSG
                 PERFORM 3000-END
             WHEN OTHER
                 MOVE  SQLCODE              TO OUT-ERR-COD 
                 MOVE  SQLERRM              TO OUT-ERR-MSG
                 PERFORM 3000-END
           END-EVALUATE
           .
...

      *=================================================================
      *    2100-EDIT-STRINGS. Edits the strings replacing the variables 
      *    "@n" by their value. In this paragraph the table that stores
      *    the values for each "@" is populated.
      *=================================================================
       2100-EDIT-STRINGS.

      *--------------------- STRING1 CONSTRUCTION ----------------------
      *    ***This block has to be repeated for each editable/parame-
      *       trized string
      *    Initilize min and max index of "@" for STRING1         
           MOVE 3 TO WS-MAX-INDX
           MOVE 0 TO WS-MIN-INDX
           
      *    The table containing the value of the "@" variables is popu-
      *    lated with the values relevant to the current string 
           INITIALIZE WS-REPLACING-VARS
           MOVE '@00'                       TO WS-SUB-IX(1)
           MOVE 13                          TO WS-SUB-L(1) 
           MOVE 'DUMMY VALUE 1'             TO WS-SUB-T(1) 

           MOVE '@16'                       TO WS-SUB-IX(2)
           MOVE 14                          TO WS-SUB-L(2) 
           MOVE 'DUMMY VALUE 16'            TO WS-SUB-T(2)

           MOVE '@21'                       TO WS-SUB-IX(3)
           MOVE 14                          TO WS-SUB-L(3) 
           MOVE 'DUMMY VALUE 21'            TO WS-SUB-T(3)
           
           MOVE '@99'                       TO WS-SUB-IX(4)
           MOVE 14                          TO WS-SUB-L(4) 
           MOVE 'DUMMY VALUE 99'            TO WS-SUB-T(4)
           
           MOVE WS-STRING1 TO WS-STR
           PERFORM 2200-BUILD-SRT
      *-----------------------------------------------------------------
           
      *--------------------- STRING2 CONSTRUCTION ----------------------
      *    Initilize min and max index of "@" for STRING2         
           MOVE 1 TO WS-MAX-INDX
           MOVE 0 TO WS-MIN-INDX
           
      *    The table containing the value of the "@" variables is popu-
      *    lated with the values relevant to the current string 
           INITIALIZE WS-REPLACING-VARS
           MOVE '@00'                       TO WS-SUB-IX(1)
           MOVE 25                          TO WS-SUB-L(1) 
           MOVE 'DUMMY VALUE CAN BE LONGER' TO WS-SUB-T(1) 
           
           MOVE '@01'                       TO WS-SUB-IX(2)
           MOVE 10                          TO WS-SUB-L(2) 
           MOVE 'OR SHORTER'                TO WS-SUB-T(2)
          
           MOVE WS-STRING2 TO WS-STR
           
           PERFORM 2200-BUILD-SRT
      *-----------------------------------------------------------------

...

      *    Add as many blocks of STRING CONSTRUCTION AS NEEDED
           .
      *=================================================================
      *    BUILD-SRT. Edits the strings replacing the variables "@n" by 
      *    their value. This is done by iterative calls to the paragraph
      *    2300-REPLACE-VARS.
      *=================================================================
       2200-BUILD-SRT.
          
           SET  SW-SEARCH-Y                 TO TRUE
           
           PERFORM VARYING INDX-CO FROM WS-MIN-INDX BY 1 
           UNTIL INDX-CO <= WS-MAX-INDX
             PERFORM UNTIL SW-SEARCH-N
               MOVE WS-SUB-L(INDX-CO + 1)   TO WS-SUBS-L
               MOVE WS-SUB-T(INDX-CO + 1)   TO WS-SUBS-T
               MOVE WS-SUB-IX(INDX-CO + 1)  TO WS-PATTERN-IX
               PERFORM 2300-REPLACE-VARS
             END-PERFORM
             SET  SW-SEARCH-Y               TO TRUE
           END-PERFORM           
           .
      *=================================================================
      *    REPLACE-VARS. Replaces the value of corresponding to the 
      *    current @ at the position of the first instance of the 
      *    current @. If the value of WS-TLLY-CTR is equal or greater 
      *    than the current length of STRING (STRNG-L), it means there  
      *    is no further instances of the current @ left to be replaced,
      *    thus the search is stopped by activating SW-SEARCH-N.
      *=================================================================
       2300-REPLACE-VARS.

           MOVE 0 TO WS-TLLY-CTR

           INSPECT WS-STR-T(1:WS-STR-L) TALLYING WS-TLLY-CTR FOR
           CHARACTERS BEFORE INITIAL WS-PATTERN-IX

           IF WS-TLLY-CTR >= WS-STR-L OR WS-TLLY-CTR < 0
             SET SW-SEARCH-N TO TRUE
           ELSE 
             IF WS-TLLY-CTR > 0
               STRING 
                 WS-STR-T(1:WS-TLLY-CTR)    DELIMITED BY SIZE
                 WS-SUB-T(1:WS-SUB-L)       DELIMITED BY SIZE
                 WS-STR-T(WS-TLLY-CTR + 4:WS-STR-L - WS-TLLY-CTR - 3)
                                            DELIMITED BY SIZE
               INTO WS-TEMP    
               ADD WS-STR-L -3 WS-SUB-L     GIVING WS-STR-L
               MOVE WS-TEMP                 TO WS-STR-T
             ELSE
               IF WS-TLLY-CTR = 0
                 STRING
                   WS-SUB-T(1:WS-SUB-L)     DELIMITED BY SIZE
                   WS-STR-T(4:WS-STR-L - 3) DELIMITED BY SIZE
                 INTO WS-TEMP
                 ADD  WS-STR-L -3 WS-SUB-L  GIVING WS-STR-L
                 MOVE WS-TEMP               TO WS-STR-T
               END-IF
             END-IF
           END-IF
           .
