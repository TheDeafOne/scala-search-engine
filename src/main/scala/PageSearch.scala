import javax.management.Query
import scala.annotation.tailrec
import scala.math.log
import scala.collection.parallel.CollectionConverters.*

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {

        def countInstances(page: RankedWebPage): Double = {
            val x = query.map(q => getMatches(page.text, q)).sum
            println(x)
            x
        }

        pages.map(countInstances)
    }

    def getMatches(str: String, keyword: String): Int = {
        @tailrec
        def helper(str: String, score: Int = 0): Int = {
            str.indexOf(keyword) match
                case -1 => score
                case i => helper(str.substring(i+keyword.length), score+1)
        }
        helper(str)
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = count(pages, query).zip(pages).map((x, y) => x/y.text.length)

    def idf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        query.map(q => log(pages.length/pages.count(p => getMatches(p.text, q) > 0)))
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val tfScores = query.map(q => tf(pages, List(q))).transpose.map(_.sum)
        val idfScores = query.map(q => idf(pages, List(q))).transpose.map(_.sum)
        tfScores.zip(idfScores).map((x, y) => x*y)
    }
}