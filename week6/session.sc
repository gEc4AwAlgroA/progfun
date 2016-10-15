val w="ABCDABCAB"
(w.toLowerCase groupBy (c => c) map {case (k, v) => (k, v.length)} toList) sortBy (_._1)


//test("wordOccurrences: abcd") {
//  assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
//}