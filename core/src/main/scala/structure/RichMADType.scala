package io.github.nordicmath.mad.structure

case class RichMADType(inner : MADType, priority : Int = 5){
    def priority(p : Int) = copy(priority = p)
}
