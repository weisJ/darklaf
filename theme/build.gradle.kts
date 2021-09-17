plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    implementation(projects.darklafPropertyLoader)
    implementation(projects.darklafUtils)

    compileOnly(projects.darklafAnnotations)
    compileOnly(libs.tools.errorprone.annotations)
    annotationProcessor(projects.darklafAnnotationsProcessor)

    compileOnly(libs.autoservice.annotations)
    annotationProcessor(libs.autoservice.processor)
}
