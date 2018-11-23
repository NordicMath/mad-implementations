package io.github.nordicmath.mad.conceptoids

import MADType._

class Conceptoid () {
    val tree = MADNavigable(Conceptoid.Conceptoid)
    
    def toJSON() = tree.toJSON()
    
    import org.json4s.native.JsonMethods._
    override def toString() = pretty(render(toJSON()))
}

object Conceptoid {    
    
    def apply() = new Conceptoid()
    
    lazy val Conceptoid = MADTree("Conceptoid", 
        "name" -> MADString,
        "collection structure" -> MADOption(Collection)
    )
    
    lazy val Collection = MADTree("Collection", 
        "representations" -> MADList(Representation)
    )
    
    lazy val Representation = MADTree("Representation",
        "description" -> MADString,
        "schema" -> MADString,
        "faithful" -> MADBool,
        "surjective" -> MADBool
    )
}
