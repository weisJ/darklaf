# Darklaf - A Darcula Look and Feel

[![CI Status](https://github.com/weisJ/darklaf/workflows/CI/badge.svg?branch=master)](https://github.com/weisJ/darklaf/actions)
[![Native Libraries](https://github.com/weisJ/darklaf/workflows/Build%20Native%20Libraries/badge.svg)](https://github.com/weisJ/darklaf/actions?query=workflow%3A%22Build+Native+Libraries%22)
![Autostyle](https://github.com/weisJ/darklaf/workflows/Autostyle/badge.svg)
[![Maven Central](https://img.shields.io/maven-central/v/com.github.weisj/darklaf-core?label=Maven%20Central)](https://search.maven.org/artifact/com.github.weisj/darklaf-core)

This project is based on the [darcula](https://github.com/bulenkov/Darcula) look and feel for Swing.

# Screenshots
|   |   |
|:-:|:-:|
|![IntelliJ](https://github.com/weisJ/darklaf/blob/master/img/file_chooser/intellij.png)|![Darcula](https://github.com/weisJ/darklaf/blob/master/img/file_chooser/darcula.png)|
|![Solarized Light](https://github.com/weisJ/darklaf/blob/master/img/file_chooser/solatized_light.png)|![Solarized Dark](https://github.com/weisJ/darklaf/blob/master/img/file_chooser/solarized_dark.png)|
|![High Contrast Light](https://github.com/weisJ/darklaf/blob/master/img/file_chooser/high_constrast_light.png)|![High Contrast Dark](https://github.com/weisJ/darklaf/blob/master/img/file_chooser/high_constrast_dark.png)|

# Usage & [Features](https://github.com/weisJ/darklaf/wiki/Features)
A list of all features can be found [here](https://github.com/weisJ/darklaf/wiki/Features).
The LaF is compatible with Java >=1.8 (you need >=1.9 to get proper scaling).

You can find all property values on [weisj.github.io/darklaf-docs](https://weisj.github.io/darklaf-docs/)

This project is available on Maven Central:
### Maven
````xml
<dependency>
  <groupId>com.github.weisj</groupId>
  <artifactId>darklaf-core</artifactId>
  <version>[2.0.3,)</version>
</dependency>
<dependency>
  <!-- For the themes-->
  <groupId>com.github.weisj</groupId>
  <artifactId>darklaf-theme</artifactId>
  <version>[2.0.3,)</version>
</dependency>
````
### Gradle
````kotlin
implementation("com.github.weisj:darklaf-core:[2.0.3,)")
implementation("com.github.weisj:darklaf-theme:[2.0.3,)") // For the themes
````

## Installation
The LookAndFeel can be easily installed using the 'LafManager'
````java
LafManager.install(); // For default theme (IntelliJ)
// or
LafManager.install(new DarculaTheme()); // Specify the used theme.
````
or by using the UIManager
````java
LafManager.setTheme(new DarculaTheme());
UIManager.setLookAndFeel(DarkLaf.class.getCanonicalName());
````

## Example
````java
import com.github.weisj.darklaf.LafManager;
import javax.swing.*;
import java.awt.*;

public class DarklafDemo {

    public static void main(final String[] args) {
         SwingUtilities.invokeLater(() -> {
            LafManager.install();

            JFrame frame = new JFrame("Darklaf - A themeable LaF for Swing");
            frame.setSize(600, 400);

            JButton button = new JButton("Click here!");

            JPanel content = new JPanel();
            content.add(button);

            frame.setLocationRelativeTo(null);
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setContentPane(content);
            frame.setVisible(true);
        });
    }
}
````

All custom component-variants and settings can be enabled by setting the appropriate client
property of the component.
They can be found [here](https://github.com/weisJ/darklaf/wiki/Features#alternative-visualsbehaviour-for-components).

This LookAndFeel supports custom window decorations (only on Windows at this point). In contrast to most other LaFs the native window behaviour is not compromised giving a seamless experience that still looks native.

# Build

    ./gradlew build

When building on Windows, you need a C++ toolchain installed to build native extension.

When building on macOS you need to have XCode (or the command line tools) installed to build the native extension.

If you want to build for platforms different from your machine you can download the respective native libraries
from the latest successful run of the [Build Native Libraries Action](https://github.com/weisJ/darklaf/actions?query=workflow%3A%22Build+Native+Libraries%22+branch%3Amaster) and place it in the corresponding `<variant>/libraries` folder.
Further details can be found in the respective `library.md` file. The libraries are then automatically included in the jar
when building the project.
When building the project there will be a message for every library that needs to be manually included.

Note: You can still build the project without the libraries, but then custom decorations won't be supported.

# Contribute
Here is a list of things that currently are not finished or need refinement. This list is a work in progress and being updated regulary. If you find any problems with the LaF feel free to submit an issue:

- [Native window decorations on Linux.](https://github.com/weisJ/darklaf/issues/2)
  * See [/windows](https://github.com/weisJ/darklaf/tree/master/windows) for the Windows implementation.
  * See [/macos(https://github.com/weisJ/darklaf/tree/master/macos) for the macOS implementation.
- [Adjustment of platform specific properties](https://github.com/weisJ/darklaf/issues/2)
  * See the [properties folder](https://github.com/weisJ/darklaf/tree/master/src/main/resources/com/github/weisj/darklaf/properties) especially the platform folder.
- Create missing demo classes. (See Todo's in test/ui/)

# Artifacts
- Group ID: `com.github.weisj`
- Version: `2.0.3`
- Artifact IDs:
  - `darklaf-core` The LaF itself. Include this in your project to use the LaF. Includes all other artifacts.
  - `darklaf-theme` The themes to use with the LaF.
  - `darklaf-property-loader` Library to load and parse property files and icon.
  - `darklaf-utils` Shared utility classes for other modules.
  - `darklaf-native-utils` Loading of native libraries.
  - `darklaf-platform-base` Common classes for the platform modules.
  - `darklaf-windows` Code specific to Windows.
  - `darklaf-macos` Code specific to macOS.



# License
This project is licensed under the [MIT license](https://github.com/weisJ/darklaf/blob/master/LICENSE).
