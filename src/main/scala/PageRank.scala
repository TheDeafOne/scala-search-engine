import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map(p => (p._1, 0.0))

//        Map() // : remove this stub and implement this method
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {

        def getPageScore(page: WebPage): Double ={
            pages.map(p => if p._2.links.contains(page.id) then 1.0 else 0).sum
        }

        pages.map(p => (p._1, getPageScore(p._2)))

//        Map() // : remove this stub and implement this method
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val rand = new Random()
        val walkLength = 100
        val numWalks = 1000

//        @tailrec
//        def kWalks(walks: Int = 0, outMap: Map[String, Double] = Map()):Map[String, Double] = {
//            println(walks)
//            if (walks >= numWalks) {
//                outMap
//            } else{
//                val walkResult = oneWalk()
//                kWalks(walks+1, outMap + (walkResult -> (outMap.getOrElse(walkResult, 0.0) + 1)))
//            }
//        }

        def kWalksParallelable(): Map[String, Double] = {
            val listOfEndStrings = (0 to numWalks).map((x) => oneWalk(0, getRandPageID).toString)
            listOfEndStrings.map((x:String) => Map(x -> 1.0)).fold(Map[String, Double]())((map,  keyString) => map + (keyString.head._1 -> (map.getOrElse(keyString.head._1, 0.0) + 1.0)))
        }


        def oneWalk(stepsTaken: Int = 0, curPageId: String = getRandPageID): String = {
            if (stepsTaken >= walkLength){
                return curPageId
            }

            val page = pages.get(curPageId) match
                case None => return oneWalk(stepsTaken+1, getRandPageID)
                case Some(i) => i
            if(page.links.length <= 0){
                return oneWalk(stepsTaken + 1, getRandPageID)
            }

            if(rand.nextDouble() < 0.85){
                oneWalk(stepsTaken+1, page.links(rand.nextInt(page.links.size)))
            }
            else {
                oneWalk(stepsTaken+1, getRandPageID)
            }
        }

        def getRandPageID: String = {
            pages.toList(rand.nextInt(pages.size))._1
        }

//        val w = kWalks()
        val w = kWalksParallelable()
        pages.map(p => (p._1, (w.getOrElse(p._1, 0.0)+1)/(numWalks + pages.size)))
//        Map() // TODO: remove this stub and implement this method
    }
}