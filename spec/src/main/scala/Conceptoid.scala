package io.github.nordicmath.mad.spec

import io.github.nordicmath.mad.structure._
import predicate._
import MADType._
import DSL._
import scala.language.postfixOps


object Conceptoid extends Spec {
    
    lazy val Bool : Pointer = mad"$Conceptoids://Booleans"
    lazy val REPR : Pointer = mad"$Conceptoids://Representations"
    
    lazy val T : Pointer = Pointer.Self(mad"$Conceptoids://")
    
    lazy val ConceptoidRef : MADRef = MADRef(mad"$Conceptoids://")
    lazy val CollectionRef : MADRef = ConceptoidRef where (mad"$Conceptoid://collection-structure" exists)
    
    def MachineRef (domain : Pointer*)(codomain : Pointer*) : RichMADType = ConceptoidRef
        .where (mad"$Conceptoid://machine-structure" exists)
        .where (mad"$Conceptoid://machine-structure/value/domain" is domain)
        .where (mad"$Conceptoid://machine-structure/value/codomain" is codomain)
    
    def top = Conceptoids
    def info = Seq(
    )
    
    lazy val Conceptoids = MADMap(Conceptoid)
    
    lazy val Conceptoid = MADTree("Conceptoid",
        "name" -> MADString,
        "description" -> MADString,
        "machine-structure" -> MADOption(Machine),
        "collection-structure" -> MADOption(Collection)
    )
    
    lazy val Collection = MADTree("Collection",
        "representations" -> MADMap(MachineRef(T)(REPR))
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
