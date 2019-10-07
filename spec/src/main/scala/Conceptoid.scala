package io.github.nordicmath.mad.spec

import io.github.nordicmath.mad._
import structure._
import predicate._
import MADType._
import DSL._
import scala.language.postfixOps


object Conceptoid extends Spec {
    
    lazy val Bool : Pointer = mad"$top://collections/Booleans"
    lazy val REPR : Pointer = mad"$top://collections/Representations"
    
    lazy val T : Pointer = Pointer.Self(mad"$top://collections")
    
    lazy val CollectionRef : MADRef = MADRef(mad"$top://collections")
    
    def MachineRef (domain : Pointer*)(codomain : Pointer*) : RichMADType = MADRef(mad"$top:/machines/")
        .where (mad"$Machine://domain" is domain)
        .where (mad"$Machine://codomain" is codomain)
    
    import Information._
    def top = MAD
    def info = Seq[Seq[Information]](
        newCollection("Booleans", "Boolean values, true or false"),
        newCollection("Representations", "The class of all representations")
    ).flatten
    
    lazy val MAD = MADTree("MAD",
        "collections" -> MADMap(Collection),
        "machines" -> MADMap(Machine)
    )
    
    
    lazy val Collection = MADTree("Collection",
        "name" -> MADString.set(category := "meta"),
        "description" -> MADString.set(category := "meta")
    )
    
    lazy val Machine = MADTree("Machine",
        "name" -> MADString.set(category := "meta"),
        "description" -> MADString.set(category := "meta"),
        "domain" -> MADList(CollectionRef),
        "codomain" -> MADList(CollectionRef),
        "defined-everywhere" -> MADBool
    )
    
    import Information._
    
    def newCollection(name : String, description : String) = Seq(
        MapNew(mad"$top://collections", name),
        Apply(mad"$top://collections/$name/name", name),
        Apply(mad"$top://collections/$name/description", description)
    )
    
    def createListSlots(path : MADPath, amt : Int) = Seq.fill(amt)(ListNew(path)) :+ ListStop(path)
    
}
