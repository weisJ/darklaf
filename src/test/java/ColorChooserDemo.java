import com.weis.darklaf.LafManager;
import com.weis.darklaf.theme.Theme;

import javax.swing.*;
import java.awt.*;

public final class ColorChooserDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            JColorChooser.showDialog(null, "Color Chooser with transparency",
                                     Color.RED, true);
//            JOptionPane.showMessageDialog(null, "This is a test!","THis is a test!", JOptionPane.ERROR_MESSAGE);
        });
    }
}
