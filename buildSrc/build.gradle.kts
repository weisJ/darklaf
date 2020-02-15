plugins {
    `kotlin-dsl`
}

dependencies {
    implementation(kotlin("gradle-plugin"))
}

repositories {
    mavenCentral()
    gradlePluginPortal()
}

configure<KotlinDslPluginOptions> {
    experimentalWarning.set(false)
}
