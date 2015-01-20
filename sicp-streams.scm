@(heading "sicp-streams")

(module sicp-streams
  @("Stream procedures from section 3.5")
  (accelerated-sequence
   cons-stream
   display-line
   display-stream
   euler-transform
   integers
   interleave
   list->stream
   make-tableau
   merge
   pairs
   scale-stream
   stream-car
   stream-cdr
   stream-enumerate-interval
   stream-filter
   stream-for-each
   stream->list
   stream-map
   stream-null
   stream-null?
   stream-ref
   the-empty-stream)
  (import chicken scheme)
  (import-for-syntax matchable)
  (use sicp)
  (include "sicp-streams-core.scm"))
