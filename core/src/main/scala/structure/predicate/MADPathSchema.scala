package io.github.nordicmath.mad.structure.predicate

import io.github.nordicmath.mad._
import structure._
import MADType._

case class MADPathSchema(prepath : MADPath) {
    val next = prepath.madtype.inner match {
        case MADList(param) => param
        case MADMap(param) => param
        case _ => throw MADException.MADTypeNotIterable(prepath.madtype)
    }
    
    
}

