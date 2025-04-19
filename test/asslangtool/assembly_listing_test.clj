(ns asslangtool.assembly-listing-test
  (:require [clojure.test :refer :all]
            [misc.testboost :refer :all]
            [asslangtool.xdada-1-2-listingfile :as lst]
            [asslangtool.assembly-listing :as alst]
            [misc.mstring :as mstr]))

(testing "`parse-label`"
  (protocol
   (alst/parse-label "                                   TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY:")
   -> mstr/->JString => "TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY"
   (alst/parse-label "                                   TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY:  ")
   -> mstr/->JString => "TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY"
   (alst/parse-label "                                   TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY :")
   => nil
   (alst/parse-label "                                   TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY: x ")
   => nil
   (alst/parse-label "                x                  TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY:")
   => nil
   (alst/parse-label "                                  TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY:")
   => nil
   (alst/parse-label "                                  TFSGCIA_GEN_OF_CLAWS_ID__3901$SECONDARY-")
   => nil
   (alst/parse-label "                                   ELSE_15:")
   -> mstr/->JString => "ELSE_15"))

(testing "`parse-instruction`"
  (let [instr (alst/parse-instruction "     000C  207C *0000000               MOVEA.L     #ADA$TFSSTODP_STORE_HANDLER_$.$DATA,A0")]
    (protocol
     (instr :address) -> mstr/->JString => "000C"
     (instr :opcode) -> mstr/->JString => "207C *0000000               "
     (instr :mnemonic) -> mstr/->JString => "MOVEA.L     "
     (instr :arguments) -> mstr/->JString => "#ADA$TFSSTODP_STORE_HANDLER_$.$DATA,A0"
     (instr :checkspace) -> :start => 1
     (instr :checkspace) -> :end => 4))
  (is (alst/parse-instruction "     000C  207C *0000000               MOVEA.L     #ADA$TFSSTODP_STORE_HANDLER_$.$DATA,A0"))
  (is (not (alst/parse-instruction "    000C  207C *0000000               MOVEA.L     #ADA$TFSSTODP_STORE_HANDLER_$.$DATA,A0")))
  (is (not (alst/parse-instruction "     000C 207C *0000000                MOVEA.L     #ADA$TFSSTODP_STORE_HANDLER_$.$DATA,A0")))
  (is (not (alst/parse-instruction "     000C 207C *0000000               MOVEA.L      #ADA$TFSSTODP_STORE_HANDLER_$.$DATA,A0")))
  (is (not (alst/parse-instruction "     0Z0C  20ZC *0000000               MOVEA.L     #ADA$TFSSTODP_STORE_HANDLER_$.$DATA,A0"))))




(testing "`assembly-listing`"

  (let [code (alst/assembly-listing
              ["     000C  4A39 *0000000               TST.B       CL1TIP.L..ADA$GMPDSTCP_COMMON_DATASTORE_$.CL1TIP"
               "                                   ELSE_15:"
               "     0012  6700 032C                   BEQ.W       ELSE_5"
               "Line 80"
               "                                   ELSE_13:"
               "     0016  4A39 *0000000               TST.B       ADA$GSYFWACP_FCC_SYSTEM_WAITS_$.$DATA+4.L..ADA$GSYFWACP_FCC_SYSTEM_WAITS_$.GSYWAIC_B010"])]
    (protocol
     code -> :labels => {"ELSE_15" 0x12, "ELSE_13" 0x16}
     code -> :code keys => [0xc 0x12 0x16]
     code -> :code (get 0xc) :address mstr/->JString => "000C"
     code -> :code (get 0x12) :address mstr/->JString => "0012"
     code -> :code (get 0x16) :address mstr/->JString => "0016"))
  
  (let [code (alst/assembly-listing
              (lst/load-listing-file "test/TestData/JCOBITCP_JCOBTCC.LIS"))]
  (protocol
   code ->> :code keys (into #{}) => #{0x0000 0x0008 0x000C 0x0012 0x0016 0x001C
                                       0x001E 0x0024 0x0028 0x002A 0x0030 0x0034
                                       0x003C 0x003E 0x0042 0x0044 0x0046 0x004A
                                       0x004C 0x0050 0x0052 0x0054 0x0058 0x005A
                                       0x005E 0x0060 0x0062 0x0064 0x0068 0x006A
                                       0x0070 0x0074 0x007C 0x0080 0x0086 0x008A
                                       0x0090 0x0340 0x0346 0x0348}
   code ->> :labels => {"JCOBTCC_BIT_TEST_CONTROLLER__301" 0x0 
                        "JCOBTCC_BIT_TEST_CONTROLLER__301$SECONDARY" 0xC
                        "ELSE_7" 0x28
                        "ELSE_15" 0x74
                        "ELSE_13" 0x340
                        "JCOBTCC_BIT_Test_Controller$EXIT_LABEL39" 0x346})))
