# Blocks-World-RBS

• Class number
	481-03

• Project number and name
	Project 1: Blocks World RBS

• Team name and members
	The A Team:
		Garinn Morton II
		Kenny Giang
		Gabriel Magallanes
		Jon Dorman
		Eric Du
		Gregory Pierot

• Intro (including the algorithm used)

	For Blocks World we utilized alogrithms known as State-Space Search & Rule-Based Systems (RBSs). In this project, we built a Blocks World RBS planner that took a Blocks World rule-set, a starting state and a goal state and constructed a sequence of gripper operations that converted the starting state into the goal state. It also employed a reasonable heuristic function to avoid “expanding” too many nodes in the search.
	More specifically, what was created was a plan – that is, a list of the states on a path from SS to GS, or more particularly, a list of the grounded rules (which are gripper "move" operations) connecting one state to the next. Moreover, during processing, a log was printed for each state "expanded" indicating the mom state, the grounded-rules generated along with their unifiers, the child state generated from each grounded-rule, and its Hamming distance to the GS, and the updated RBS search state of the Open and Closed lists.


• Contents: Files in the .zip submission
	-blockWorld.el
	-README.txt

• External Requirements (None?)
	N/A

• Setup and Installation (if any)
	N/A

• Sample invocation (A* Root-to-Goal Path)

((:state rbs-424) (:ham-dist 0) (:wmem ((is-on A B) (hand-empty) (is-clear A) (is-on B C) (is-on C D) (is-ontable D))) (:mom-state rbs-421))

((:state rbs-421) (:ham-dist 5) (:wmem ((is-gripping A) (is-on B C) (is-clear B) (is-on C D) (is-ontable D))) (:mom-state rbs-420))

((:state rbs-420) (:ham-dist 3) (:wmem ((is-on B C) (hand-empty) (is-clear B) (is-on C D) (is-clear A) (is-ontable A) (is-ontable D))) (:mom-state rbs-416))

((:state rbs-416) (:ham-dist 6) (:wmem ((is-gripping B) (is-on C D) (is-clear C) (is-clear A) (is-ontable A) (is-ontable D))) (:mom-state rbs-413))

((:state rbs-413) (:ham-dist 6) (:wmem ((is-on C D) (hand-empty) (is-clear C) (is-ontable B) (is-clear B) (is-clear A) (is-ontable A) (is-ontable D))) (:mom-state rbs-408))

((:state rbs-408) (:ham-dist 9) (:wmem ((is-gripping C) (is-ontable B) (is-clear B) (is-clear D) (is-clear A) (is-ontable A) (is-ontable D))) (:mom-state rbs-359))

((:state rbs-359) (:ham-dist 9) (:wmem ((hand-empty) (is-ontable B) (is-clear B) (is-clear D) (is-ontable C) (is-clear C) (is-clear A) (is-ontable A) (is-ontable D))) (:mom-state rbs-358))

((:state rbs-358) (:ham-dist 9) (:wmem ((is-gripping B) (is-clear D) (is-ontable C) (is-clear C) (is-clear A) (is-ontable A) (is-ontable D))) (:mom-state rbs-351))

((:state rbs-351) (:ham-dist 8) (:wmem ((hand-empty) (is-ontable C) (is-clear C) (is-clear A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D))) (:mom-state rbs-350))

((:state rbs-350) (:ham-dist 8) (:wmem ((is-gripping C) (is-clear A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D))) (:mom-state rbs-477))

((:state rbs-477) (:ss t) (:ham-dist 9) (:wmem ((is-clear C) (is-on C A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D) (hand-empty))))
nil

• Features (both included and missing)
	Global Variables:
		rbs-open
		rbs-closed
		rbs-parentages
		rbs-rules
		rbs-goal

	Fucntions (bottom-up):
		(defun is-rbs-var (rx))
		(defun pairlis (ra rb))
		(defun cs-lhs-unify (rexpr-1 rexpr-2))
		(defun get-all-rule-unifiers-1 (rpre-cond rwmem))
		(defun asubst (ralist rxlist))
		(defun get-all-rule-unifiers (rpre-conds rwmem))
		(defun ham-dist (ralst rblst))
		(defun replace-vars-w-vals (rexpr ralist))
		(defun get-rule-conds (gnd-rule))
		(defun remove-gnd-rule-conds (gnd-conds wmem))
		(defun get-rule-adds (gnd-rule))
		(defun add-gnd-rule-adds(gnd-adds middle-wmem))
		(defun add-parentage (kid mom))
		(defun make-kid-wmem (mom-wmem gnd-rule))
		(defun make-kid (mom gnd-rule))
		(defun make-gnd-rule (rule rule-unifier))
		(defun get-state-wmem (mom))
		(defun get-kids-for-rules (mom-state))
		(defun is-on-open (kid-state open))
		(defun is-on-closed (kid-state closed))
		(defun add-mom-new-kids-to-open (new-kid-states))
		(defun get-mom-new-kids (mom-state))
		(defun add-mom-to-closed (mom-state))
		(defun get-next-mom (goal-state))
		(defun print-root-goal-path ())
		(defun rbs-1-step (goal-state))
		(defun is-goal (state))
		(defun run-rbs ())		

• Bugs (if any)
	N/A
