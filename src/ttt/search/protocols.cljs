(ns ttt.search.protocols)

(defprotocol ISearchable
  (-search-compare [this compare-to]))

