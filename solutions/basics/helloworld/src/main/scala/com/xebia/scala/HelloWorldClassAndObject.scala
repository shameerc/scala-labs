package com.xebia.scala

object HelloWorldClassAndObject {
	def apply(initialText:String):HelloWorldClassAndObject = {
		new HelloWorldClassAndObject {
			val text=initialText
		}
	}
}

abstract class HelloWorldClassAndObject {
	val text:String
	def echo:String = text
}