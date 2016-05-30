;;; graph.el --- Draw directed acyclic graphs in ascii

;;; Commentary:
;;
;; Gansner, Emden R., et al. "A technique for drawing directed graphs."
;; Software Engineering, IEEE Transactions on 19.3 (1993): 214-230.
;;
;; The drawing algorithm draws a attributed graph
;; G = (V,E)
;; Graph is a set of Nodes (V) and Edges (E) possibly containing loops.
;; xsize(v),ysize(v): size of the bounding box of node (v)
;; nodesep(G): mininum separation of nodes with the same rank
;; ranksep(G): minimum separation of nodes with different rank
;; w(e): weight of an edge (e), usually 1. Greater weights keep edges more aligned and shorter.

;; The algorithm has 4 main passes:
;; 1. place nodes in discrete ranks with a network simplex algorithm
;; 2. order nodes within ranks to avoid edge crossing.
;; 3. set actual layout coordinates of nodes
;; 4. draw edges

;; (defun draw-graph ()
;;   (rank)
;;   (ordering)
;;   (position)
;;   (draw-edges))

;; 1. Assign each node (v) in (G) an integer rank lambda(v) consistent with its edges.
;; For every edge e = (v,w) in E,l(e) ge delta(e), where length l(e) of e=(v,w)
;; is defined as lambda(w) - lambda(v), and delta(e) represents some given minimum length constraint.
;; delta(e) can be any nonnegative integer value.
;; Muliedges are disregarded for this implementation.
;; Leaf nodes are ignored because it's trivially determined in an optimal ranking.

;; A graph must be acyclic to have consistent rank assignment. Because the input
;; graph may contain cycles, a preprocessor detects cycles and breaks them by reversing certain edges internally.
;; Edges are searched in the natural order of the graph input, starting from some source or sink nodes if
;; any exist. Depth-first search partitions edges into 2 sets:
;; tree and nontreeedges. The tree defines apartial order on nodes.
;; Given this partial oder, the nontree edges further partition into 3 sets:
;; 1. cross edges, connecting nodes unrelated in the partial order.
;; 2. forward edges, connecting a node with some of its decendant.
;; 3. back edges, connect a descendant to some of its ancestors.
;; Reversing back edges (internally (not for drawing) and for science) breaks the cycles.
;; Because its hard to find minimal set of edges to reverse which wouldn't improve the drawings,
;; a heuristic reversed edges that participate in many cycles.
;; It looks at one nontivial strongly connected component at a time, in an arbitrary order.
;; For each it counds the number of times each edge forms a cycle in a depth-first traveral.
;; An edge with a maximal count is reversed. This is repeated until there are no more nontrivial strongly
;; connected components.
;;
;; Finding an optimal node ranking will result in one where the sum of all weighted edge lenghts is minimal.
;; min SUM((v,w) in E) w(v,w)(lambda(w)-lambda(v))
;; subject to: lambda(w) - lambda(v) ge delta(v,w) for all (v,w) in E
;; a.k.a minimal sum over all edges with weight of edge times difference of lambda(w) and lambda(v)
;; so that delta of all edges is smaller than lambda(w) - lambda(v)
;;
;; Solving it:
;; A feasible ranking is one satisfying the length constraints l(e) ge delta(e) for all e.
;; The slack is the difference of its length and its minimum length.
;; A ranking is feasible if the slack of every edge is nonnegative.
;; An edge is tight if its slack is zero.
;; Pick an initial node and assign it a rank.
;; Then for each adjacent node assign it the jank of the adjacent node, incremented or decremented
;; by the minimum length of the connecting edge, depending n whether it is the head or tail of the connecting
;; edge.
;; So a tailing node gets an incremented rank, a head node a decremented.
;; A spanning tree is feasible if it induces a feasible ranking. By construction, all edges in the feasible tree
;; are tight.
;; With that tree we can assign an integer cut value with each edge:
;; If the edge is deleted, the tree breaks into two connected components, the tail component
;; containing the tail node of the edge and the head component containing the head node.
;; Cut value is the sum of weights of all edges from tail  to the head component, including the tree edge,
;; minus the sum of the weights of all edges from head component to the tail component.
;;
;; A negative cut value indicates that the weighted edge length sum could be reduced by lengthening the
;; tree edge as much as possible, until on of the head component to tail component edges becomes tight.
;; In practice you shouldn't be bothered with a termination of the algorithm but it could happen.
;;
;; (defun rank ()
;;   (feasible-tree)
;;   (let (e)
;;     (while (setq e (leave_edge))
;;       (exchange e (enter-edge e))))
;;   (normalize)
;;   (balance))
;;
;; `feasible-tree' constructs an initial feasible spanning tree.
;; `leave-edge' returns a tree edge with a negative cut value or nil if there is none, meaning we did our job.
;; Any edge with a negative cut value may be selected as the edge to remove.
;; `enter-edge' finds a nontree edge to replace e. This is done by breaking the edge e,
;; which divides the tree into a head and tail component.
;; All edges going from head to tail are considered, with an edge of minimum slack being chosen.
;; Important to maintain feasibility.
;; `exchange' replaces e with the chosen edge, updating the tree and its cut values.
;; `normalize' sets the least rank to zero and decrements all other ranks by that offset.
;; `balance' will take nodes with equal in and out edge weights and multiple feasible ranks and moves them to a
;; rank with the fewest nodes. This reduces crowding and improves the aspect ration of the graph.
;; Adjustment is done in a greedy fashion, which works good enough.
;;
;; (defun feasible-tree ()
;;   (init-rank)
;;   (while (< (tight-tree) (absolute-value V))
;;     (let ((e (get-non-tree-node-incident-on-tree-with-min-slack))
;;           (delta (slack e)))
;;       (when (eq incident-node (e-head e))
;;         (setq delta (- delta)))
;;       (mapc
;;        (lambda (v) (setf (v-rank v) (+ (v-rank v) delta)))
;;        tree)))
;;   (init-cutvalues))
;;
;; `init-rank' keeps a node queue. Nodes are placed in the queue when they have no unscanned in-edges.
;; As nodes are taken off the queue, they are assigned the least rank that satisfies their in-edges and
;; their out-edges are marked as scanned. In the simplest case where delta is 1 for all edges this
;; corresponds to viewing the graph as a poset, assigning the minimal elements to rank 0. These nodes
;; are removed from the poset and the new set of minimal elements are assigned rank 1, etc.
;; `tight-tree' finds a maximal tree of tight edges containing some fixed node and returns the number
;; of nodes in the tree. Note that such a maximal tree is just a spanning tree for the subgraph induced by all
;; nodes reachable from the fixed node in the underlying undirected graph using only tight edges.
;; All such trees have the same number of nodes.
;; The while loop finds an edge to a nontree node that is adjacent to the tree and adjusts the ranks of the
;; ranks of the tree nodes to make this edge tight.
;; Because the edge had minimal slack the result is still feasible.
;; With each iteration the `tight-tree' will return at least one more because the tree grew by one or more nodes.
;; It eventually terminates with a feasible spanning tree. I guess when all nodes are in the tree.
;; `init-cutvalues' computes the cut values of treeedges.
;; All nodes belonging to the head or tail component are marked and the sum of the signed weights of all
;; edges whose head and tail are in different components, the sign being negative for those edges going from the
;; head to the tail component.
;;
;; Ok so `tight-tree' we take any node from the tree after `init-rank'.
;; It can literally be any, but should stay fixed.
;; From that node we traverse downstream every tight edge (slack is 0).
;; E.g.
;;
;; a -- b -- c -- d
;;   \------ f -/
;;      g -/     0
;;
;; If we take a we traverse right. The edge(a,f) is not tight so f won't be part of the tight tree.
;; edge(a,b) is tight as well as edge(b,c) and edge(c,d) so a -- b -- c -- is our tight tree.
;; Then we take a node from outside the tree that is connected to the tree.
;; So we go through all edges of all nodes and find all unvisited edges.
;; We gonna find the edge(a,f) and edge(f,d).
;; edge(f,d) has zero slack so it's better.

;;; Code:
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'cl))

(cl-defstruct node label edges visisted)
(cl-defstruct edge tail head)
(cl-defstruct graph nodes)

(define-error 'graph-error "Something in the graph went wrong")
(define-error 'graph-value-error "Value given is wrong")
(define-error 'node-not-in-graph "Node is not part of Graph" 'graph-error)

(defun init-graph ()
  "Create a graph with a root node."
  (make-graph :nodes (list (make-node :label "root"))))

(defun root (graph)
  "Return the root of GRAPH."
  (car (graph-nodes graph)))

(defun add-node (node graph &optional head)
  "Connect a new NODE to HEAD node of GRAPH.
If HEAD is nil, use the root node of GRAPH."
  (let* ((nodes (graph-nodes graph))
         (head (or head (root graph))))
    (unless (member head nodes)
      (signal 'node-not-in-graph (list head graph)))
    (setf (graph-nodes graph) (add-to-list 'nodes node t))
    (add-edge node head graph)))

(defun add-edge (from to graph)
  "Add edge FROM node TO node in GRAPH."
  (let ((edge (make-edge :tail from :head to))
        (edges (node-edges from)))
    (setf (node-edges from) (add-to-list 'edges edge t))))

(defun get-node (label nodes)
  "Return node that has LABEL from NODES."
  (when nodes
    (cl-find label nodes :key 'node-label :test 'equal)))

(defun can-add-node (label dependencies graph)
  "Return t if all dependencies are in graph."
  (let ((nodes (graph-nodes graph)))
    (if dependencies
        (when (= (length dependencies)
                 (length (remove nil (mapcar (lambda (dep) (get-node dep nodes)) dependencies))))
          t)
      t)))

(defun create-dependency-graph (nodealist)
  "Create a graph from a alist of labels with dependencies.

(list (list \"a\" \"b\" \"c\") (list \"b\") (list \"c\")) will create a dependency graph:

root -- b -- a
    \\-- c --/
"
  (let ((g (init-graph))
        (nodealist (sort nodealist (lambda (a b) (< (length a) (length b)))))) ; nodes with less dependencies first
    (while nodealist
      (mapc
       (lambda (labeldependencies)
         (let* ((label (car labeldependencies))
                (dependencies (cdr labeldependencies))
                (node (make-node :label label))
                (nodes (graph-nodes g)))
           (when (can-add-node label dependencies g)  ;; are all dependencies already in the graph?
             (add-node node g (get-node (car dependencies) nodes))  ;; if (car dependencies) is nil use root
             (mapc
              (lambda (dependency)
                (add-edge node (get-node dependency nodes) g))
              (cdr dependencies))
             (setq nodealist (delete labeldependencies nodealist)))))
       nodealist))
    g))

(provide 'graph)
;;; graph.el ends here
