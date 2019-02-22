package io.github.nordicmath.mad.spec

import io.github.nordicmath.mad.structure._
import predicate._
import MADType._
import DSL._
import scala.language.postfixOps


object Conceptoid {
    
    lazy val Bool : Pointer = mad"$Conceptoids://Booleans"
    lazy val REPR : Pointer = mad"$Conceptoids://Representations"
    
    lazy val T : Pointer = Pointer.Self(mad"$Conceptoids://")
    
    lazy val ConceptoidRef : MADRef = MADRef(mad"$Conceptoids://")
    lazy val CollectionRef : MADRef = ConceptoidRef where (mad"$Conceptoid://collection-structure" exists)
    
    lazy val Conceptoids = MADMap(Conceptoid)
    
    lazy val Conceptoid = MADTree("Conceptoid", 
        "name" -> MADString,
        "description" -> MADString,
        "machine-structure" -> MADOption(Machine),
        "collection-structure" -> MADOption(Collection)
    )
    
    lazy val Collection = MADTree("Collection", 
        "representations" -> MADMap(Representation)
    )
    
    lazy val Machine = MADTree("Machine", 
        "domain" -> MADList(CollectionRef), 
        "codomain" -> MADList(CollectionRef), 
        "defined-everywhere" -> MADBool, 
        "algorithm" -> MADList(Algorithm)
    )
    
    lazy val Algorithm = MADTree("Algorithm",
        "javascript" -> MADString
    )
    
    lazy val Representation = MADTree("Representation",
        "description" -> MADString,
        "schema" -> MADString,
        "faithful" -> MADBool,
        "surjective" -> MADBool
    )
}
