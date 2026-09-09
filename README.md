;;;=====================================================================
;;; ISO-AUTOMATION.LSP
(vl-load-com)
;;; Tools to convert FLATSHOT output into a colored pipe isometric:
;;;   - Pipe geometry  -> layer PIPE-ISO  (green, color 3)
;;;   - Equipment      -> layer EQUIP-ISO (pink/magenta, color 6)
;;;   - Quick "NNN NB / XX BEND" leader tags on the correct layer
;;;=====================================================================

(defun ensure-layer (lname color / )
  (if (not (tblsearch "LAYER" lname))
    (command "_.-layer" "_N" lname "_C" color lname "")
  )
)

;; ---- PIPE (green) ----------------------------------------------------

(defun c:FS-PIPE ( / ss)
  (ensure-layer "PIPE-ISO" 3)
  (princ "\nSelect flatshot PIPE geometry to recolor to PIPE-ISO (green): ")
  (setq ss (ssget))
  (if ss
    (progn
      (command "_.chprop" ss "" "_LA" "PIPE-ISO" "_C" "BYLAYER" "")
      (princ "\nDone - entities moved to PIPE-ISO.")
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(defun c:FS-PIPE-EXPLODE ( / ss i)
  (ensure-layer "PIPE-ISO" 3)
  (princ "\nSelect flatshot BLOCK(S) (pipe) to explode + recolor to PIPE-ISO: ")
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (command "_.explode" (ssname ss i))
        (command "_.chprop" "_P" "" "_LA" "PIPE-ISO" "_C" "BYLAYER" "")
        (setq i (1+ i))
      )
      (princ "\nDone.")
    )
    (princ "\nNo blocks selected.")
  )
  (princ)
)

;; ---- EQUIPMENT (pink/magenta) ----------------------------------------

(defun c:FS-EQUIP ( / ss)
  (ensure-layer "EQUIP-ISO" 6)
  (princ "\nSelect flatshot EQUIPMENT geometry to recolor to EQUIP-ISO (pink): ")
  (setq ss (ssget))
  (if ss
    (progn
      (command "_.chprop" ss "" "_LA" "EQUIP-ISO" "_C" "BYLAYER" "")
      (princ "\nDone - entities moved to EQUIP-ISO.")
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(defun c:FS-EQUIP-EXPLODE ( / ss i)
  (ensure-layer "EQUIP-ISO" 6)
  (princ "\nSelect flatshot BLOCK(S) (equipment) to explode + recolor to EQUIP-ISO: ")
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (command "_.explode" (ssname ss i))
        (command "_.chprop" "_P" "" "_LA" "EQUIP-ISO" "_C" "BYLAYER" "")
        (setq i (1+ i))
      )
      (princ "\nDone.")
    )
    (princ "\nNo blocks selected.")
  )
  (princ)
)

;; ---- TRUE MATCH PROPERTIES (no explode) --------------------------------

(defun c:FS-MATCH-PIPE ( / src dest)
  (ensure-layer "PIPE-ISO" 3)
  (princ "\nSelect SOURCE object already on PIPE-ISO (green): ")
  (setq src (entsel))
  (princ "\nSelect DESTINATION flatshot block(s) to match: ")
  (setq dest (ssget))
  (if (and src dest)
    (progn
      (command "_.matchprop" (car src) dest "")
      (princ "\nDone - properties matched to PIPE-ISO.")
    )
    (princ "\nSelection incomplete - nothing changed.")
  )
  (princ)
)

(defun c:FS-MATCH-EQUIP ( / src dest)
  (ensure-layer "EQUIP-ISO" 6)
  (princ "\nSelect SOURCE object already on EQUIP-ISO (pink): ")
  (setq src (entsel))
  (princ "\nSelect DESTINATION flatshot block(s) to match: ")
  (setq dest (ssget))
  (if (and src dest)
    (progn
      (command "_.matchprop" (car src) dest "")
      (princ "\nDone - properties matched to EQUIP-ISO.")
    )
    (princ "\nSelection incomplete - nothing changed.")
  )
  (princ)
)

;; ---- COMBINED EXPLODE + RECOLOR ---------------------------------------

(defun c:FS-EXPLODE ( / typ lay col ss i)
  (initget "Pipe Equipment")
  (setq typ (getkword "\nExplode as [Pipe/Equipment] <Pipe>: "))
  (if (not typ) (setq typ "Pipe"))
  (if (= typ "Pipe")
    (progn (setq lay "PIPE-ISO") (setq col 3))
    (progn (setq lay "EQUIP-ISO") (setq col 6))
  )
  (ensure-layer lay col)
  (princ (strcat "\nSelect flatshot BLOCK(S) to explode + recolor to " lay ": "))
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (command "_.explode" (ssname ss i))
        (command "_.chprop" "_P" "" "_LA" lay "_C" "BYLAYER" "")
        (setq i (1+ i))
      )
      (princ (strcat "\nDone - exploded and moved to " lay "."))
    )
    (princ "\nNo blocks selected.")
  )
  (princ)
)

;; ---- QUICK ONE-CLICK TAG (placeholder text, edit values after) --------
(setq *NBTAGDIST* 40)
(setq *NBTAGANGLE* 45)

(defun dtr (a) (* pi (/ a 180.0)))

(defun c:QNB-PIPE ( / p1 p2 oldclayer)
  (ensure-layer "PIPE-ISO" 3)
  (setq oldclayer (getvar "CLAYER"))
  (princ "\nClick on the pipe: ")
  (setq p1 (getpoint))
  (if p1
    (progn
      (setq p2 (polar p1 (dtr *NBTAGANGLE*) *NBTAGDIST*))
      (setvar "CLAYER" "PIPE-ISO")
      (command "_.leader" p1 p2 "" "### NB" (strcat "XX" (chr 176) " BEND") "")
      (setvar "CLAYER" oldclayer)
      (princ "\nPlaceholder tag placed - double-click the text (DDEDIT) to fix size/angle.")
    )
    (princ "\nNo point picked.")
  )
  (princ)
)

(defun c:QNB-EQUIP ( / p1 p2 oldclayer)
  (ensure-layer "EQUIP-ISO" 6)
  (setq oldclayer (getvar "CLAYER"))
  (princ "\nClick on the equipment nozzle/connection: ")
  (setq p1 (getpoint))
  (if p1
    (progn
      (setq p2 (polar p1 (dtr *NBTAGANGLE*) *NBTAGDIST*))
      (setvar "CLAYER" "EQUIP-ISO")
      (command "_.leader" p1 p2 "" "### NB" (strcat "XX" (chr 176) " BEND") "")
      (setvar "CLAYER" oldclayer)
      (princ "\nPlaceholder tag placed - double-click the text (DDEDIT) to fix size/angle.")
    )
    (princ "\nNo point picked.")
  )
  (princ)
)

;; ---- QUICK TAG, CHOOSE ANGLE EACH TIME ---------------------------------

(defun c:QNB-PIPE-A ( / ang p1 p2 oldclayer)
  (ensure-layer "PIPE-ISO" 3)
  (setq oldclayer (getvar "CLAYER"))
  (setq ang (getreal (strcat "\nLeader angle in degrees <" (rtos *NBTAGANGLE* 2 0) ">: ")))
  (if (not ang) (setq ang *NBTAGANGLE*))
  (princ "\nClick on the pipe: ")
  (setq p1 (getpoint))
  (if p1
    (progn
      (setq p2 (polar p1 (dtr ang) *NBTAGDIST*))
      (setvar "CLAYER" "PIPE-ISO")
      (command "_.leader" p1 p2 "" "### NB" (strcat "XX" (chr 176) " BEND") "")
      (setvar "CLAYER" oldclayer)
      (princ "\nPlaceholder tag placed - double-click the text (DDEDIT) to fix size/angle.")
    )
    (princ "\nNo point picked.")
  )
  (princ)
)

(defun c:QNB-EQUIP-A ( / ang p1 p2 oldclayer)
  (ensure-layer "EQUIP-ISO" 6)
  (setq oldclayer (getvar "CLAYER"))
  (setq ang (getreal (strcat "\nLeader angle in degrees <" (rtos *NBTAGANGLE* 2 0) ">: ")))
  (if (not ang) (setq ang *NBTAGANGLE*))
  (princ "\nClick on the equipment nozzle/connection: ")
  (setq p1 (getpoint))
  (if p1
    (progn
      (setq p2 (polar p1 (dtr ang) *NBTAGDIST*))
      (setvar "CLAYER" "EQUIP-ISO")
      (command "_.leader" p1 p2 "" "### NB" (strcat "XX" (chr 176) " BEND") "")
      (setvar "CLAYER" oldclayer)
      (princ "\nPlaceholder tag placed - double-click the text (DDEDIT) to fix size/angle.")
    )
    (princ "\nNo point picked.")
  )
  (princ)
)

;; ---- BATCH: TAG EVERY SELECTED SEGMENT IN ONE GO -----------------------

(defun get-ent-center (ent / obj minp maxp)
  (setq obj (vlax-ename->vla-object ent))
  (vlax-invoke obj 'GetBoundingBox 'minp 'maxp)
  (setq minp (vlax-safearray->list minp))
  (setq maxp (vlax-safearray->list maxp))
  (list (/ (+ (car minp) (car maxp)) 2.0)
        (/ (+ (cadr minp) (cadr maxp)) 2.0)
        (/ (+ (caddr minp) (caddr maxp)) 2.0)
  )
)

(defun tag-set (ss lay / n i ent p1 p2 oldclayer oldosmode)
  (setq oldclayer (getvar "CLAYER"))
  (setq oldosmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setvar "CLAYER" lay)
  (setq n (sslength ss) i 0)
  (repeat n
    (setq ent (ssname ss i))
    (setq p1 (get-ent-center ent))
    (setq p2 (polar p1 (dtr *NBTAGANGLE*) *NBTAGDIST*))
    (command "_.leader" p1 p2 "" "### NB" (strcat "XX" (chr 176) " BEND") "")
    (setq i (1+ i))
  )
  (setvar "CLAYER" oldclayer)
  (setvar "OSMODE" oldosmode)
  n
)

(defun c:QNB-ALL-PIPE ( / ss n)
  (ensure-layer "PIPE-ISO" 3)
  (princ "\nSelect ALL pipe segments/blocks to tag (window/crossing), then Enter: ")
  (setq ss (ssget))
  (if ss
    (progn
      (setq n (tag-set ss "PIPE-ISO"))
      (princ (strcat "\n" (itoa n) " placeholder tags placed - fix each with DDEDIT (double-click text)."))
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(defun c:QNB-ALL-EQUIP ( / ss n)
  (ensure-layer "EQUIP-ISO" 6)
  (princ "\nSelect ALL equipment segments/blocks to tag (window/crossing), then Enter: ")
  (setq ss (ssget))
  (if ss
    (progn
      (setq n (tag-set ss "EQUIP-ISO"))
      (princ (strcat "\n" (itoa n) " placeholder tags placed - fix each with DDEDIT (double-click text)."))
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

;; ---- IN-BLOCK WORKFLOW (run these WHILE inside REFEDIT) ----------------

(defun c:PIPEFIX ( / ss ss2 ans n)
  (ensure-layer "PIPE-ISO" 3)
  (princ "\n[Run this WHILE inside REFEDIT - double-click the block first]")
  (princ "\nSelect the pipe lines inside the block to recolor: ")
  (setq ss (ssget))
  (if ss
    (progn
      (command "_.chprop" ss "" "_LA" "PIPE-ISO" "_C" "BYLAYER" "")
      (princ "\nRecolored to PIPE-ISO (green).")
    )
    (princ "\nNothing selected - skipping recolor.")
  )
  (initget "Yes No")
  (setq ans (getkword "\nAlso drop NB/BEND placeholder tags now? [Yes/No] <Yes>: "))
  (if (not ans) (setq ans "Yes"))
  (if (= ans "Yes")
    (progn
      (princ "\nSelect the pipe segments to tag (can be the same selection again): ")
      (setq ss2 (ssget))
      (if ss2
        (progn
          (setq n (tag-set ss2 "PIPE-ISO"))
          (princ (strcat "\n" (itoa n) " placeholder tags placed - fix each with DDEDIT."))
        )
        (princ "\nNothing selected - skipping tags.")
      )
    )
  )
  (princ "\nSaving changes back into the block...")
  (command "_.refclose" "_Save")
  (princ)
)

(defun c:EQUIPFIX ( / ss ss2 ans n)
  (ensure-layer "EQUIP-ISO" 6)
  (princ "\n[Run this WHILE inside REFEDIT - double-click the block first]")
  (princ "\nSelect the equipment lines inside the block to recolor: ")
  (setq ss (ssget))
  (if ss
    (progn
      (command "_.chprop" ss "" "_LA" "EQUIP-ISO" "_C" "BYLAYER" "")
      (princ "\nRecolored to EQUIP-ISO (pink).")
    )
    (princ "\nNothing selected - skipping recolor.")
  )
  (initget "Yes No")
  (setq ans (getkword "\nAlso drop NB/BEND placeholder tags now? [Yes/No] <Yes>: "))
  (if (not ans) (setq ans "Yes"))
  (if (= ans "Yes")
    (progn
      (princ "\nSelect the equipment segments to tag (can be the same selection again): ")
      (setq ss2 (ssget))
      (if ss2
        (progn
          (setq n (tag-set ss2 "EQUIP-ISO"))
          (princ (strcat "\n" (itoa n) " placeholder tags placed - fix each with DDEDIT."))
        )
        (princ "\nNothing selected - skipping tags.")
      )
    )
  )
  (princ "\nSaving changes back into the block...")
  (command "_.refclose" "_Save")
  (princ)
)

;; ---- NB SIZE / BEND TAG (typed-in version) ---

(defun c:ISOTAG ( / typ lay sz bend txt1 txt2 p1 p2 oldclayer)
  (setq oldclayer (getvar "CLAYER"))
  (initget "Pipe Equipment")
  (setq typ (getkword "\nTag type [Pipe/Equipment] <Pipe>: "))
  (if (not typ) (setq typ "Pipe"))
  (if (= typ "Pipe")
    (setq lay "PIPE-ISO")
    (setq lay "EQUIP-ISO")
  )
  (ensure-layer lay (if (= lay "PIPE-ISO") 3 6))
  (setq sz (getstring "\nEnter pipe/nozzle size (e.g. 250): "))
  (initget "Straight 45 90 Custom")
  (setq bend (getkword "\nBend angle [Straight/45/90/Custom] <Straight>: "))
  (if (not bend) (setq bend "Straight"))
  (if (= bend "Custom")
    (setq bend (getstring "\nEnter bend angle in degrees: "))
  )
  (setq txt1 (strcat sz " NB"))
  (setq txt2 (if (= bend "Straight") "" (strcat bend (chr 176) " BEND")))
  (princ "\nSpecify leader start point (arrow, at the pipe/fitting): ")
  (setq p1 (getpoint))
  (princ "\nSpecify text location: ")
  (setq p2 (getpoint p1))
  (setvar "CLAYER" lay)
  (if (= txt2 "")
    (command "_.leader" p1 p2 "" txt1 "")
    (command "_.leader" p1 p2 "" txt1 txt2 "")
  )
  (setvar "CLAYER" oldclayer)
  (princ)
)

;; ---- HELP ---------------------------------------------------------------

(defun c:ISOTOOLS ()
  (princ "\n--- ISO AUTOMATION COMMANDS ---")
  (princ "\nFS-PIPE          : recolor selected flatshot pipe entities -> PIPE-ISO (green), no explode")
  (princ "\nFS-EQUIP         : recolor selected flatshot equipment -> EQUIP-ISO (pink), no explode")
  (princ "\nFS-MATCH-PIPE    : pick a green source object, match its properties onto flatshot pipe block(s)")
  (princ "\nFS-MATCH-EQUIP   : pick a pink source object, match its properties onto flatshot equipment block(s)")
  (princ "\nFS-PIPE-EXPLODE  : explode flatshot pipe block(s) then recolor -> PIPE-ISO")
  (princ "\nFS-EQUIP-EXPLODE : explode flatshot equipment block(s) then recolor -> EQUIP-ISO")
  (princ "\nFS-EXPLODE       : one command - asks Pipe/Equipment, then explode + recolor")
  (princ "\nQNB-PIPE         : ONE CLICK - drops '### NB / XX BEND' placeholder (pipe, green), fixed angle")
  (princ "\nQNB-EQUIP        : ONE CLICK - drops placeholder (equipment, pink), fixed angle")
  (princ "\nQNB-PIPE-A       : type an angle, then one click - placeholder tag (pipe, green)")
  (princ "\nQNB-EQUIP-A      : type an angle, then one click - placeholder tag (equipment, pink)")
  (princ "\nQNB-ALL-PIPE     : BATCH - select all pipe segments at once, tags every one automatically")
  (princ "\nQNB-ALL-EQUIP    : BATCH - select all equipment segments at once, tags every one automatically")
  (princ "\nPIPEFIX          : run INSIDE REFEDIT - recolor + batch-tag pipe lines + auto-save block")
  (princ "\nEQUIPFIX         : run INSIDE REFEDIT - recolor + batch-tag equipment lines + auto-save block")
  (princ "\nISOTAG           : type size/bend up front, inserts finished leader text (pipe or equipment)")
  (princ)
)

(princ "\nISO-AUTOMATION.LSP loaded. Type ISOTOOLS for command list.")
(princ)
