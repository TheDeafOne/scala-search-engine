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
            query.map(q => getMatches(page.text, q)).sum
        }

        pages.map(countInstances)
//        List() // : implement this method and remove this stub
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
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }
}