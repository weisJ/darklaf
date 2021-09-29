plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    api(projects.darklafUtils)
    implementation(libs.jsvg)
    implementation(libs.visualPaddings)
    compileOnly(libs.nullabilityAnnotations)
    compileOnly(toolLibs.errorprone.annotations)
    testImplementation(testLibs.junit.api)
    testRuntimeOnly(testLibs.junit.engine)
}

tasks.test {
    useJUnitPlatform()
}
