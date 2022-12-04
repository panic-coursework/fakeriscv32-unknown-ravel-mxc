import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
  kotlin("jvm") version "1.7.10"
  antlr
  application
}

group = "org.altk.lab"
version = "0.0-SNAPSHOT"

repositories {
  mavenCentral()
}

dependencies {
  antlr("org.antlr:antlr4:4.11.1")
  implementation("org.jetbrains.kotlin:kotlin-reflect:1.7.10")
}

tasks.withType<KotlinCompile> {
  kotlinOptions.jvmTarget = "11"
}

application {
  mainClass.set("org.altk.lab.mxc.MainKt")
}

tasks.getByName("generateGrammarSource") {
  this as AntlrTask
  arguments = arguments + listOf("-package", "org.altk.lab.mxc.recognizer", "-visitor")
}

tasks.compileKotlin {
  dependsOn("generateGrammarSource")
}
