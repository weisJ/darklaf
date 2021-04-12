apply(from= "../gradle/loadProps.gradle.kts")

plugins {
    `kotlin-dsl`
    `java-gradle-plugin`
}

val nokeeVersion = extra["nokee.version"]

dependencies {
    implementation(platform("dev.nokee:nokee-gradle-plugins:$nokeeVersion"))
    implementation(gradleApi())
}

repositories {
    mavenCentral()
    gradlePluginPortal()
    maven { url = uri("https://dl.bintray.com/nokeedev/distributions") }
    maven { url = uri("https://dl.bintray.com/nokeedev/distributions-snapshots") }
}

gradlePlugin {
    plugins {
        create("uber-jni-jar") {
            id = "uber-jni-jar"
            implementationClass = "UberJniJarPlugin"
        }
        create("use-prebuilt-binaries") {
            id = "use-prebuilt-binaries"
            implementationClass = "UsePrebuiltBinariesWhenUnbuildablePlugin"
        }
    }
}
