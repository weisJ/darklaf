# Darklaf - A Darcula Look and Feel

This project is based on the [darcula](https://github.com/bulenkov/Darcula) look and feel for Swing.
Gradually all the custom UI and more from the darcula project wil be ported over and given a new look that follows the newer darcula look in IntelliJ.


# Screenshot
<p align="center">
<img src="https://raw.githubusercontent.com/weisJ/darklaf/master/img/color_chooser.png" width="400">
</p>

# Usage & Features
The LookAndFeel can be easily installed using the 'LafManager'

````java
LafManager.install();
````

or by any other default method of setting the LaF of your swing application.

## Example
````java
import com.weis.darklaf.LafManager;
import javax.swing.*;
import java.awt.*;

public class DarklafDemo {

    public static void main(String[] args) {
         SwingUtilities.invokeLater(() -> {
            LafManager.install();

            JFrame frame = new JFrame("Darklaf - A Darcula LaF for Swing");
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

All custom component-variants and settings are customizable by setting the appropriate client
property of the component.

//Todo: list of properties

This LookAndFeel supports custom window decorations (only Windows at this point). In contrast to most other LaFs the native window behaviour is not compromised giving a seamless experience that still looks native.

# Roadmap
- Finish all ui classes for the dark variant.
- Port/Create light variant.
- Enable custom theming.
