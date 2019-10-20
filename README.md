# Darklaf - A Darcula Look and Feel

This project is based on the [darcula](https://github.com/bulenkov/Darcula) look and feel for Swing.
Gradually all the custom UI and more from the darcula project wil be ported over and given a new look that follows the newer darcula look in IntelliJ.


# Screenshot
<img src="https://user-images.githubusercontent.com/31143295/67161757-f6d25b80-f35d-11e9-9d95-c2cad1bb99ce.png" width="45%"></img> <img src="https://user-images.githubusercontent.com/31143295/67161758-f6d25b80-f35d-11e9-906d-0a2710e046ca.png" width="45%"></img> <img src="https://user-images.githubusercontent.com/31143295/67161759-f6d25b80-f35d-11e9-93c0-9dc5c365e8d5.png" width="45%"></img> <img src="https://user-images.githubusercontent.com/31143295/67161760-f76af200-f35d-11e9-91f1-fcfc62eb9e23.png" width="45%"></img>

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
