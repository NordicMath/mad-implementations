package io.github.nordicmath.mad.questions

import io.github.nordicmath.mad._

import memory._
import conceptoids._

object PriorityEngine {
    def generatePath()(implicit mem : Memory) : Path = {
        val paths = generatePaths()
        
        if(paths.length > 0) paths.apply(0) else throw MADException("No questions!")
    }
    
    def generatePaths()(implicit mem : Memory) : Seq[Path] = for {
        (cname, conceptoid) <- mem.getObjects
        sub <- conceptoid.tree.subpaths
        path = Path(cname, sub)
        if !mem.getAttribute(path).isset
    } yield path
}
