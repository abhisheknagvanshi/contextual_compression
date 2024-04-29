       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW21.
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
           COPY    "COMMON-SPA".
           COPY    "W01COMMON-SPA".
           COPY    "ENUM-VALUE".
           COPY    "W20SCR-SPA".
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-NYUKHN                          PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-BRM-NUM                         PIC 9(01).
           03  FLG-KIKAN-DABURI                    PIC 9(01).
           03  FLG-UPD-KBN                         PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-PTNYUINRRK                      PIC 9(01).
           03  FLG-SYS5002                         PIC 9(01).
           03  FLG-TARGET                          PIC 9(01).
           03  FLG-INPUT-AREA                      PIC 9(01).
           03  FLG-SSTKIJUNERR                     PIC 9(01).
           03  FLG-KSNERR                          PIC 9(01).
           03  FLG-HIT                             PIC 9(01).
           03  FLG-NYUKSNCHK                       PIC 9(01).
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDX6                                PIC 9(05).
           03  IDX7                                PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDXB                                PIC 9(05).
           03  IDXL                                PIC 9(05).
           03  IDXS                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-BRMLST                          PIC 9(05).
           03  IDX-KSN                             PIC 9(05).
           03  IDX-GKSN                            PIC 9(05).
           03  IDX-SST                             PIC 9(05).
           03  IDX-MUL                             PIC 9(05).
       01  CNT-AREA.
           03  CNT-SST                             PIC 9(05).
           03  CNT-SEL                             PIC 9(05).
           03  CNT-KSN                             PIC 9(05).
       01  WRK-AREA.
           03  WRK-MCP-WIDGET                      PIC X(64). 
           03  WRK-WIDMSG                          PIC X(70). 
           03  WRK-MSGPLUS                         PIC X(20). 
           03  WRK-MAX                             PIC 9(05).
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-KANACHK-INPUT               PIC X(400).
           03  WRK-SYS5002-KBNCD.
               05  WRK-SYS5002-BTUNUM             PIC X(02).
               05  WRK-SYS5002-BRMNUM             PIC X(06).
           03  WRK-ZZZ9                            PIC ZZZ9.
           03  WRK-999999                          PIC 9(06).
           03  WRK-ZZZZZ9                          PIC Z(5)9.
           03  WRK-ZZZZZZZ9                        PIC Z(7)9.
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
               05  WRK-CMB-ITM2                    PIC X(200).
               05  WRK-CMB-ITM3                    PIC X(200).
               05  WRK-CMB-ITM4                    PIC X(200).
               05  WRK-CMB-ITM5                    PIC X(200).
           03  WRK-KJNYMD                          PIC X(08).
           03  WRK-STR1                            PIC X(20).
           03  WRK-STR2                            PIC X(20).
           03  WRK-STR-TMP                         PIC X(100).
           03  WRK-IDX1                            PIC 9(05).
           03  WRK-IDX2                            PIC 9(05).
           03  WRK-IDX3                            PIC 9(05).
           03  WRK-BRMNUM                          PIC X(06).
           03  WRK-BRMNUM-KEY                      PIC X(06).
           03  WRK-BRMNUM-IN                       PIC X(06).
           03  WRK-BRMNUM-OT                       PIC X(06).
           03  WRK-TOKNYUIN                        PIC X(02).
           03  WRK-KHNSRYCD                        PIC X(09).
           03  WRK-KJN-YUKSTYMD                    PIC X(08).
           03  WRK-BTU-YUKSTYMD                    PIC X(08).
           03  WRK-YUKOSTYMD                       PIC X(08).
           03  WRK-YUKOEDYMD                       PIC X(08).
           03  WRK-TNSYMD                          PIC X(08).
           03  WRK-TNSSRYCD                        PIC X(09).
           03  WRK-HEN-SAGAKU.
               05  WRK-HEN-SAGAKU-Z9               PIC ZZZZZ9.
               05  WRK-HEN-SAGAKU-EN               PIC X(02).
           03  WRK-KSNCD                           PIC X(09).
           03  WRK-KSNIDX                          PIC 9(03).
           03  WRK-KSNKBN                          PIC 9(03).
           03  WRK-NYUINKBN                        PIC 9(03).
           03  WRK-SSTKJNCD-G.
               05  WRK-SSTKJNCD-OCC                OCCURS 10.
                   07  WRK-SSTKJNCDSEP             PIC X(01).
                   07  WRK-SSTKJNCD                PIC X(04).
                   07  WRK-SSTKJNCD-R              REDEFINES
                       WRK-SSTKJNCD.
                       09  WRK-SSTKJNCD9           PIC 9(04).
           03  WRK-KSNMEI                          PIC X(100).
           03  WRK-PNT                             PIC 9(03).
           03  WRK-BTUNUM                          PIC X(02).
           03  WRK-SELNUM-G.
               05  WRK-SELNUM-MUL                  PIC X(50).
               05  WRK-INPUT-MODE                  PIC 9(01).
               05  WRK-SELNUM-X                    PIC X(50).
               05  WRK-SELNUM                      PIC 9(03).
               05  WRK-SELNUM-FLG                  PIC 9(01)
                                                   OCCURS  200.
           03  WRK-MULTI-AREA.
               05  WRK-MUL-NUM             PIC X(10)
                                           OCCURS 20.
               05  WRK-MUL-TENSU-X         PIC X(10).
               05  WRK-MUL-STNUM-X         PIC X(10).
               05  WRK-MUL-EDNUM-X         PIC X(10).
               05  WRK-MUL-TENSU           PIC 9(05).
               05  WRK-MUL-STNUM           PIC 9(05).
               05  WRK-MUL-EDNUM           PIC 9(05).
           03  WRK-ZZZ-G.
               05  WRK-ZZZ                 PIC ZZZ.
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-YUKOSTYMD-MOT           PIC X(08).
           03  WRK-NUM02X.
               05  WRK-NUM02                       PIC 9(02).
           03  WRK-FROM                            PIC 9(05).
           03  WRK-KBN1-G.
               05  WRK-KBN1                        PIC 9(01).
           03  WRK-KBN2-G.
               05  WRK-KBN2                        PIC 9(02).
           03  WRK-KBN4-G.
               05  WRK-KBN4                        PIC 9(04).
           03  WRK-TOKCHK1-G.
               05  WRK-TOKCHK1-KBNCD               PIC X(02)
                                                   OCCURS 100.
           03  WRK-TOKCHK2-G.
               05  WRK-TOKCHK2-KBNCD               PIC X(02)
                                                   OCCURS 100.
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
       01  TKSN-AREA.
           03  TKSN-MAX                            PIC 9(03).
           03  TKSN-OCC                            OCCURS  7.
               05  TKSN-LBL                        PIC X(36).
               05  TKSN-IDX                        PIC 9(03).
               05  TKSN-SRYCD                      PIC X(09).
       01  SSTKJN-AREA.
           03  SSTKJN-VAL.
               05  SSTKJN-VAL-OCC                  PIC X(500)
                                                   OCCURS  20.
           03  SSTKJN-R    REDEFINES       SSTKJN-VAL.
               05  SSTKJN-CD                       PIC X(01)
                                                   OCCURS  10000.
       01  TSPLIT-CONF-DATE-AREA.
           03  TSPLIT-CONF-DATE-OCC                OCCURS  4.
               05  TSPLIT-CONF-DATE-BEFORE         PIC 9(01).
               05  TSPLIT-CONF-DATE-AFTER          PIC 9(01).

       01  WRK-GMN-CHIIKIHOU.
           03  FILLER                      PIC X(43)   VALUE
               "01 E+-$".
           03  FILLER                      PIC X(02)   VALUE   "FK".
           03  FILLER                      PIC X(02)   VALUE   "FO".
           03  FILLER                      PIC X(04)   VALUE   "0001".
           03  FILLER                      PIC X(459)  VALUE   SPACE.
           03  FILLER                      PIC X(43)   VALUE
               "01 E+-$".
           03  FILLER                      PIC X(02)   VALUE   "FC".
           03  FILLER                      PIC X(02)   VALUE   "FG".
           03  FILLER                      PIC X(04)   VALUE   "0010".
           03  FILLER                      PIC X(43)   VALUE
               "02 E+-$".
           03  FILLER                      PIC X(02)   VALUE   "FM".
           03  FILLER                      PIC X(02)   VALUE   "FQ".
           03  FILLER                      PIC X(04)   VALUE   "0001".
           03  FILLER                      PIC X(43)   VALUE
               "03 E+-$".
           03  FILLER                      PIC X(02)   VALUE   "FY".
           03  FILLER                      PIC X(02)   VALUE   "FS".
           03  FILLER                      PIC X(04)   VALUE   "0011".
           03  FILLER                      PIC X(357)  VALUE   SPACE.
      *                     
           03  FILLER                      PIC X(43)   VALUE
               "01 E+-$".
           03  FILLER                      PIC X(02)   VALUE   "F4".
           03  FILLER                      PIC X(02)   VALUE   "F8".
           03  FILLER                      PIC X(04)   VALUE   "0100".
           03  FILLER                      PIC X(459)  VALUE   SPACE.
      *     
           03  FILLER                      PIC X(43)   VALUE
               "01 E+-$".
           03  FILLER                      PIC X(02)   VALUE   "F6".
           03  FILLER                      PIC X(02)   VALUE   "FA".
           03  FILLER                      PIC X(04)   VALUE   "0100".
           03  FILLER                      PIC X(43)   VALUE
               "02 E+-$".
           03  FILLER                      PIC X(02)   VALUE   "FE".
           03  FILLER                      PIC X(02)   VALUE   "FI".
           03  FILLER                      PIC X(04)   VALUE   "0010".
           03  FILLER                      PIC X(43)   VALUE
               "03 E+-$".
           03  FILLER                      PIC X(02)   VALUE   "FW".
           03  FILLER                      PIC X(02)   VALUE   "FU".
           03  FILLER                      PIC X(04)   VALUE   "0110".
           03  FILLER                      PIC X(357)  VALUE   SPACE.
       01  WRK-GMN-CHIIKIHOU-R        REDEFINES   WRK-GMN-CHIIKIHOU.
           03  WRK-GMN-CHIIKIHOULST-O      OCCURS   4.
               05  WRK-GMN-CHIIKIHOUL-OCC      OCCURS  10.
                   07  WRK-GMN-CHIIKIHOULST.
                       09  WRK-GMN-CHIIKIHOUL  PIC X(02).
                       09  WRK-GMN-CHIIKIHOUFL PIC X(01).
                       09  WRK-GMN-CHIIKIHOUMEIL
                                               PIC X(40).
                   07  WRK-GMN-CHIIKIHOUL-KBN
                                           PIC X(02)   OCCURS  2.
                   07  WRK-GMN-CHIIKIHOUL-CD
                                           PIC X(04).
       01  CONST-AREA.
           03  CONST-SPLIT-CONF-DATE-VAL.
               05  CONST-H200401            PIC X(08) VALUE "20080401".
               05  CONST-H220401            PIC X(08) VALUE "20100401".
               05  CONST-H240401            PIC X(08) VALUE "20120401".
               05  CONST-R020401            PIC X(08) VALUE "20200401".
               05  CONST-R040401            PIC X(08) VALUE "20220401".
            03  CONST-SPLIT-CONF-DATE-R REDEFINES
                                            CONST-SPLIT-CONF-DATE-VAL.
               05  CONST-SPLIT-CONF-DATE    PIC X(08) OCCURS 5.
           03  CONST-SPLIT-CONF-DATE-MAX    PIC 9(03) VALUE  5.
           03  CONST-H180401                PIC X(08) VALUE "20060401".
           03  CONST-H180701                PIC X(08) VALUE "20060701".
           03  CONST-H250401                PIC X(08) VALUE "20130401".
           03  CONST-H241001                PIC X(08) VALUE "20121001".
           03  CONST-KSN-MAX                PIC 9(03) VALUE 11.
           03  CONST-GKSN-MAX               PIC 9(03) VALUE 14.
           03  CONST-5002-KSN-MAX           PIC 9(03) VALUE 12.
           03  CONST-TOKNYUIN-MAX           PIC 9(03) VALUE 50.
           03  CONST-CHIIKIHOU-MAX          PIC 9(03) VALUE 10.
           03  CONST-R02-AREA.
               05   CONST-R02-A301C5        PIC X(09) VALUE "190219270".
           03  CONST-H24-AREA.
               05   CONST-H2404-BTURYO1     PIC X(09) VALUE "190120210".
               05   CONST-H2404-BTURYO2     PIC X(09) VALUE "190106070".
               05   CONST-H2404-BTURYO3     PIC X(09) VALUE "190106170".
               05   CONST-H2404-BTURYO4     PIC X(09) VALUE "190106270".
               05   CONST-H2404-SNRRYO1     PIC X(09) VALUE "190106370".
               05   CONST-H2404-SNRRYO2     PIC X(09) VALUE "190106470".
               05   CONST-H2404-BTURYOKAI1  PIC X(09) VALUE "190146210".
               05   CONST-H2404-BTURYOKAI2  PIC X(09) VALUE "190146310".
               05   CONST-H2404-SNRRYOKAI   PIC X(09) VALUE "190146410".
               05   CONST-H2404-MUKIN1      PIC X(09) VALUE "190106570".
               05   CONST-H2404-MUKIN1K     PIC X(09) VALUE "190713410".
               05   CONST-H2404-MUKIN2      PIC X(09) VALUE "190146510".
           03  CONST-SINSEIJI          PIC X(09) VALUE "190109770".
           03  CONST-SNR-RYOYO-VAL.
               05                      PIC X(09) VALUE "190106370".
               05                      PIC X(09) VALUE "190106470".
           03  CONST-SNR-RYOYO-R REDEFINES   CONST-SNR-RYOYO-VAL.
               05  CONST-SNR-RYOYO
                                       PIC X(09) OCCURS 2.
           03  CONST-HOSYASEN          PIC X(09) VALUE "190106670".
           03  CONST-RYOYO             PIC X(09) VALUE "190105570".
           03  CONST-BTU-RYOYO-VAL.
               05                      PIC X(09) VALUE "190106070".
               05                      PIC X(09) VALUE "190106170".
               05                      PIC X(09) VALUE "190106270".
           03  CONST-BTU-RYOYO-R   REDEFINES   CONST-BTU-RYOYO-VAL.
               05  CONST-BTU-RYOYO     PIC X(09) OCCURS 3.
           03  CONST-BTU-RYOYO-H18     PIC X(09) VALUE "190120210".
           03  CONST-JYUTOK1           PIC X(09) VALUE "190105870".
           03  CONST-JYUTOK2           PIC X(09) VALUE "190105970".
           03  CONST-MUKIN             PIC X(09) VALUE "190106570".
           COPY  "CPSYSKANRI.INC".
           COPY  "CPSK1001.INC".
           COPY  "CPSK1006.INC".
           COPY  "CPSK5001.INC".
           COPY  "CPSK5002.INC".
           COPY  "CPSK1005.INC".
           COPY  "CPSK5108.INC".
           COPY  "CPSK5110.INC".
           COPY  "CPSK5111.INC".
           COPY  "CPSK5113.INC".
           COPY  "CPSK5109.INC".
           COPY  "CPTENSU.INC".
       01  PTNYUINRRK-REC.
           COPY  "CPPTNYUINRRK.INC".
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
           COPY    "CPSK5002.INC"  REPLACING  //SYS-//
                                   BY         //TMP-//.
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSNUM.INC".
           COPY    "CPORCSGDAY.INC".
           COPY    "MCPDATA.INC".
           COPY    "CPORCSKANACHK.INC".
           COPY    "CPORCSBRMNUM.INC".
           COPY    "CPORCSMCNDATE.INC".
           COPY    "CPORCSADDSIGN.INC".
       LINKAGE                     SECTION.
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
       000-PROC-SEC                SECTION.
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W20FREE
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
           END-EVALUATE.
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
           MOVE    SPA-W20FREE     TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
           .
           EXIT    PROGRAM
           .
       100-INIT-SEC                SECTION.
           IF    ( SPA-MOTOPG      =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W21    "           TO  MCP-WINDOW.
           PERFORM 900-PUT-WINDOW.
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
       300-SCREEN-SEC              SECTION.
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
           EVALUATE    SPA-MOTOPG
           WHEN    "W20"
               IF    ( SPA-W20-W21-MOTOPG  =   "W21" )
                   INITIALIZE  SPA-W20-W21-AREA
                   PERFORM 3101-SYS-5001-GET-SEC
                   MOVE    SPA-GMN-W21-BTUNUM
                                       TO  WRK-CMB-CD
                   PERFORM 4200-BTUNUMLST-CHK-SEC
                   IF    ( WRK-CMB-ITM =   SPACE )
                       GO  TO  300-SCREEN-EXT
                   END-IF
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-BTUNUM-G
                   MOVE    WRK-CMB-ITM2    TO  WRK-TOKNYUIN
                   MOVE    WRK-CMB-ITM3    TO  WRK-KHNSRYCD
                   MOVE    WRK-CMB-ITM4    TO  WRK-BTU-YUKSTYMD
                   MOVE    WRK-CMB-ITM5    TO  SPA-NAI-W21-BTUSBT
                   PERFORM 3101-BTU-KHNRYO-EDIT-SEC
                   PERFORM 4100-SYSKANRI-RESET-SEC
                   GO  TO  300-SCREEN-EXT
               ELSE
                   PERFORM 310-SPASET-SEC
                   PERFORM 310-BTU-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
       310-SPASET-SEC              SECTION.
           INITIALIZE              SPA-W21-AREA
           MOVE    SPA-SYSYMD      TO  SPA-NAI-W21-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-W21-KJNYMD
           PERFORM 311-SPASET-SEC
           .
       310-SPASET-EXT.
           EXIT.
       311-SPASET-SEC              SECTION.
           PERFORM 3101-SYS-5001-GET-SEC
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           MOVE    1               TO  SPA-GMN-W21-CUR
           .
       311-SPASET-EXT.
           EXIT.
       310-BTU-SET-SEC                 SECTION.
           PERFORM 420-CLEAR-SEC
           MOVE    SPA-W20-W21-BTUNUM
                                   TO  WRK-CMB-CD
           PERFORM 4200-BTUNUMLST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               GO  TO  310-BTU-SET-EXT
           END-IF
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-BTUNUM-G
           MOVE    WRK-CMB-ITM2    TO  WRK-TOKNYUIN
           MOVE    WRK-CMB-ITM3    TO  WRK-KHNSRYCD
           MOVE    WRK-CMB-ITM4    TO  WRK-BTU-YUKSTYMD
           MOVE    WRK-CMB-ITM5    TO  SPA-NAI-W21-BTUSBT
           PERFORM 3101-BTU-KHNRYO-EDIT-SEC
           PERFORM 3101-SYS-5002-GET-SEC
           IF    ( SPA-GMN-W21-BRMLST-MAX  >   ZERO )
               MOVE    2           TO  SPA-GMN-W21-CUR
           ELSE
               MOVE    3           TO  SPA-GMN-W21-CUR
           END-IF
           .
       310-BTU-SET-EXT.
           EXIT.
       3101-BTU-KHNRYO-EDIT-SEC    SECTION.
           INITIALIZE              SPA-GMN-W21-BTUKHNRYO
                                   SPA-NAI-W21-BTUKHNCD
                                   SPA-NAI-W21-BTUTOKCD
           IF    ( WRK-KHNSRYCD    NOT =   SPACE )
               MOVE    WRK-KHNSRYCD        TO  WRK-TNSSRYCD
               MOVE    WRK-BTU-YUKSTYMD    TO  WRK-TNSYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME        TO  SPA-GMN-W21-BTUKHNRYO
               MOVE    WRK-KHNSRYCD    TO  SPA-NAI-W21-BTUKHNCD
           END-IF
           IF    ( WRK-TOKNYUIN    NOT =   SPACE )
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5110"          TO  SYS-KANRICD
               MOVE    WRK-TOKNYUIN    TO  SYS-KBNCD
               MOVE    WRK-BTU-YUKSTYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC     TO  SYS-5110-REC
               MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                        TO  SPA-GMN-W21-BTUKHNRYO
               MOVE    SYS-5110-SRYCD   TO  SPA-NAI-W21-BTUTOKCD
           END-IF
           .
       3101-BTU-KHNRYO-EDIT-EXT.
           EXIT.
       3101-CMB-SYOKI-SEC          SECTION.
           PERFORM 3101-SYS-1005-GET-SEC
           PERFORM 3101-SYS-5108-GET-SEC
           PERFORM 3101-SYS-5111-GET-SEC
           PERFORM 3101-SYS-5113-GET-SEC
           PERFORM 3101-SYS-5109-GET-SEC
           IF    ( WRK-KJN-YUKSTYMD       >=   CONST-H180701 )
            AND  ( SPA-NAI-W21-BTUSBT      =   "04" OR "09" )
               PERFORM 3101-KHNRYO-GET-SEC
           END-IF
           PERFORM 3101-TBTUTYPE-GET-SEC
           PERFORM 3101-CMB-KSNCOMMON-SET-SEC
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H240401 )
               PERFORM 31012-CMB-KSN-H24R02-SEC
           WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H220401 )
               PERFORM 31012-CMB-KSN-H22-SEC
           WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H180401 )
               PERFORM 31012-CMB-KSN-SYOKI-SEC
           WHEN    OTHER
               PERFORM 31011-CMB-KSN-SYOKI-SEC
           END-EVALUATE
           .
       3101-CMB-SYOKI-EXT.
           EXIT.
       3101-SYS-5001-GET-SEC       SECTION.
           INITIALIZE  SPA-GMN-W21-BTUNUMLST-G
                       SPA-NAI-W21-BTUINFLST-G
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-NAI-W21-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
           PERFORM UNTIL ( FLG-SYSKANRI        NOT =   ZERO )
                    OR   ( SPA-GMN-W21-BTUNUM-MAX >=   50 )
               COMPUTE SPA-GMN-W21-BTUNUM-MAX
                   =   SPA-GMN-W21-BTUNUM-MAX  +   1
               MOVE    SPA-GMN-W21-BTUNUM-MAX
                                   TO  IDX1
               MOVE    SYS-5001-KBNCD
                                   TO  SPA-GMN-W21-BTUNUML (IDX1)
               MOVE    SYS-5001-BTU-NAME
                                   TO  SPA-GMN-W21-BTUNUMMEIL    (IDX1)
               MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                   TO  SPA-NAI-W21-BTUINF-TOKNYUIN
                                                                 (IDX1)
               MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  SPA-NAI-W21-BTUINF-KHNSRYCD
                                                                 (IDX1)
               MOVE    SYS-5001-STYUKYMD
                                   TO  SPA-NAI-W21-BTUINF-STYUKYMD
                                                                 (IDX1)
               MOVE    SYS-5001-BTU-SBT
                                   TO  SPA-NAI-W21-BTUINF-SBT    (IDX1)
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       3101-SYS-5001-GET-EXT.
           EXIT.
       3101-SYS-1005-GET-SEC       SECTION.
           INITIALIZE  SPA-GMN-W21-SRYKALST-G
           INITIALIZE  SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-SRYKA-MAX   >=  99 )
               COMPUTE SPA-GMN-W21-SRYKA-MAX
                   =   SPA-GMN-W21-SRYKA-MAX   +   1
               MOVE    SPA-GMN-W21-SRYKA-MAX
                                       TO  IDX1
               MOVE    SYS-1005-KBNCD
                                   TO  SPA-GMN-W21-SRYKAL (IDX1)
               MOVE    SYS-1005-SRYKANAME
                                   TO  SPA-GMN-W21-SRYKAMEIL (IDX1)
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       3101-SYS-1005-GET-EXT.
           EXIT.
       3101-SYS-5108-GET-SEC       SECTION.
           INITIALIZE  SPA-GMN-W21-SBTLST-G
           INITIALIZE  SYSKANRI-REC
           MOVE    "5108"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5108-REC
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-SBT-MAX >=  15 )
               COMPUTE SPA-GMN-W21-SBT-MAX
                   =   SPA-GMN-W21-SBT-MAX +   1
               MOVE    SPA-GMN-W21-SBT-MAX
                                   TO  IDX1
               MOVE    SYS-5108-KBNCD
                                   TO  SPA-GMN-W21-SBTL   (IDX1)
               MOVE    SYS-5108-BRM-SBT-NM
                                   TO  SPA-GMN-W21-SBTMEIL(IDX1)
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5108-REC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       3101-SYS-5108-GET-EXT.
           EXIT.
       3101-SYS-5111-GET-SEC       SECTION.
           INITIALIZE              SPA-GMN-W21-TOKNYUINLST-G
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5111"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5111-REC
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-TOKNYUIN-MAX
                                          >=  CONST-TOKNYUIN-MAX )
               IF    ( SYS-5111-BRM-NYUINRYOKBN    =  "01"  )
                AND  ( SYS-5111-SEIKATU-RYOKBN     =  SPACE )
                   COMPUTE SPA-GMN-W21-TOKNYUIN-MAX
                       =   SPA-GMN-W21-TOKNYUIN-MAX    +   1
                   MOVE    SPA-GMN-W21-TOKNYUIN-MAX
                                       TO  IDX1
                   MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                   TO  SPA-GMN-W21-TOKNYUINMEIL
                                                            (IDX1)
                   MOVE    SYS-5111-KBNCD
                                   TO  SPA-GMN-W21-TOKNYUINL-KBNCD
                                                            (IDX1)
                   MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM2
                                   TO  SPA-GMN-W21-TOKNYUINL-TANMEI
                                                            (IDX1)
                   MOVE    SYS-5111-SRYCD
                                   TO  SPA-GMN-W21-TOKNYUINL-SRYCD
                                                            (IDX1)
                   MOVE    SYS-5111-SRYCD          TO  WRK-TNSSRYCD
                   MOVE    WRK-KJN-YUKSTYMD        TO  WRK-TNSYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-KOUHYOJYUNNUM
                                       TO  SPA-GMN-W21-TOKNYUINL-JUNNUM
                                                         (IDX1)
               END-IF
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           COMPUTE IDXS    =   CONST-TOKNYUIN-MAX  +   1
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-W21-TOKNYUIN-MAX )
               COMPUTE WRK-FROM    =   IDXA    +   1
               PERFORM VARYING IDXB    FROM    WRK-FROM   BY  1
                       UNTIL ( IDXB    >   SPA-GMN-W21-TOKNYUIN-MAX )
                   IF    ( SPA-GMN-W21-TOKNYUINL-JUNNUM (IDXA)
                               >   SPA-GMN-W21-TOKNYUINL-JUNNUM (IDXB))
                       MOVE    SPA-GMN-W21-TOKNYUINLST-OCC (IDXA)
                               TO  SPA-GMN-W21-TOKNYUINLST-OCC (IDXS)
                       MOVE    SPA-GMN-W21-TOKNYUINLST-OCC (IDXB)
                               TO  SPA-GMN-W21-TOKNYUINLST-OCC (IDXA)
                       MOVE    SPA-GMN-W21-TOKNYUINLST-OCC (IDXS)
                               TO  SPA-GMN-W21-TOKNYUINLST-OCC (IDXB)
                   END-IF
               END-PERFORM
               COMPUTE WRK-NUM02   =   IDXA
               MOVE    WRK-NUM02X  TO  SPA-GMN-W21-TOKNYUINL (IDXA)
           END-PERFORM
           .
       3101-SYS-5111-GET-EXT.
           EXIT.
       3101-SYS-5113-GET-SEC       SECTION.
           INITIALIZE  SPA-GMN-W21-SAGAKULST-G
           INITIALIZE  SYSKANRI-REC
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5113-REC
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-SAGAKU-MAX  >=  99 )
               COMPUTE SPA-GMN-W21-SAGAKU-MAX
                   =   SPA-GMN-W21-SAGAKU-MAX  +   1
               MOVE    SPA-GMN-W21-SAGAKU-MAX
                                   TO  IDX1
               MOVE    SYS-5113-KBNCD
                                   TO  SPA-GMN-W21-SAGAKUL (IDX1)
               INITIALIZE              WRK-HEN-SAGAKU
               MOVE    SYS-5113-BRM-SAGAKU-NM
                                   TO  WRK-HEN-SAGAKU-Z9
               MOVE    "±ß"        TO  WRK-HEN-SAGAKU-EN
               MOVE    WRK-HEN-SAGAKU
                                   TO  SPA-GMN-W21-SAGAKUMEIL (IDX1)
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       3101-SYS-5113-GET-EXT.
           EXIT.
       3101-SYS-5109-GET-SEC       SECTION.
           INITIALIZE  SPA-GMN-W21-SEXLST-G
           INITIALIZE  SYSKANRI-REC
           MOVE    "5109"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5109-REC
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-SEX-MAX >=  10 )
               COMPUTE SPA-GMN-W21-SEX-MAX
                   =   SPA-GMN-W21-SEX-MAX +   1
               MOVE    SPA-GMN-W21-SEX-MAX
                                   TO  IDX1
               MOVE    SYS-5109-KBNCD
                                   TO  SPA-GMN-W21-SEXL (IDX1)
               MOVE    SYS-5109-BRM-SEX-NM
                                   TO  SPA-GMN-W21-SEXMEIL (IDX1)
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5109-REC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       3101-SYS-5109-GET-EXT.
           EXIT.
  