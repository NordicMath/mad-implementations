package io.github.nordicmath.mad.conceptoids

import io.github.nordicmath.mad.structure._
import MADType._


object Conceptoid {    
    
    lazy val Conceptoids = MADMap(Conceptoid)
    
    lazy val Conceptoid = MADTree("Conceptoid", 
        "name" -> MADString,
        "description" -> MADString,
        "collection-structure" -> MADOption(Collection),
        "machine-structure" -> MADOption(Machine)
    )
    
    lazy val Collection = MADTree("Collection", 
        "representations" -> MADMap(Representation)
    )
    
    lazy val Machine = MADTree("Machine", 
        //"domain" -> MADList(MADRef where collection),
        //"codomain" -> MADList(MADRef where collection),
        "defined-everywhere" -> MADBool
        //"algorithm" -> MADList(Algorithm)
    )
    
    lazy val Representation = MADTree("Representation",
        "description" -> MADString,
        "schema" -> MADString,
        "faithful" -> MADBool,
        "surjective" -> MADBool
    )
}
