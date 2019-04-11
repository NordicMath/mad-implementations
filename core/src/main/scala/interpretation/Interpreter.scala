package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._
import structure._
import Information._
import MADException._
import memory._
import predicate._


trait Interpreter {
    def interpret (str : String) : Information
}

object Interpreter {
    
    def parseBool(str : String) : Boolean = str match {
        case "true" | "yes" => true
        case "false" | "no" => false
        case _ => throw BooleanInput
    }
    
    def parseInt(str : String) : Int = try str.toInt catch {
        case _ : IllegalArgumentException => throw IntegerInput
    }
    
    def parseMADPath(str : String, madtype : RichMADType) : MADPath = {
        val prefix = "mad://"
        
        if(!str.startsWith(prefix)) throw MADException.MADPathInput
        val instr = str.drop(prefix.length).split("/")
        val instructions : Seq[String] = instr.filter(_.length > 0)
        
        MADPath(madtype, instructions)
    }
    
    
    def stringInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[String](path, str)
    }
    
    def intInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[Int](path, parseInt(str))
    }
    
    def boolInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[Boolean](path, parseBool(str))
    }
    
    def optionInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = OptionAssign(path, parseBool(str))
    }
    
    def listInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = if (parseBool(str)) ListNew(path) else NoInformation
    }
    
    def mapInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = MapNew(path, str)
    }
    
    def enumInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = EnumAssign(path, parseInt(str))
    }
    
    def pathInterpreter(path : MADPath)(implicit mem : Memory) = new Interpreter {
        private[this] val madtype = path.madtype.inner.asInstanceOf[MADType.MADRef]
        private[this] val MADType.MADRef(schema, predicate) = madtype
        def interpret (str : String) = {
            val value = parseMADPath(str, schema.madtype)
            if(!schema.check(value)) throw MADException.SchemaFailMADPath
            
            val nav = util.Try(mem.getObject(value)).getOrElse(throw MADException.UndefinedMADPath)
            if(!predicate.fold(true)(Predicate.eval(nav, _))) throw MADException.PredicateFailMADPath
            
            ReferenceApply(path, value)
        }
    }
}
