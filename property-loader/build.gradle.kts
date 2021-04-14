plugins {
    `java-library`
}

dependencies {
    api(projects.darklafUtils)
    implementation(libs.svgSalamander)
    testImplementation(libs.test.junit.api)
    testRuntimeOnly(libs.test.junit.engine)
}

tasks.test {
    useJUnitPlatform()
}
