plugins {
    `java-library`
}

dependencies {
    implementation(projects.darklafPropertyLoader)
    implementation(projects.darklafUtils)

    compileOnly(projects.darklafAnnotations)
    annotationProcessor(projects.darklafAnnotationsProcessor)

    compileOnly(libs.autoservice.annotations)
    annotationProcessor(libs.autoservice.processor)
}
