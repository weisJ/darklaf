import com.weis.darklaf.LafManager;

import javax.swing.*;
import java.awt.*;

public final class ColorChooserDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.loadLaf(LafManager.Theme.Dark);
            JColorChooser.showDialog(null, "Color Chooser with transparency",
                                     Color.RED, true);
//            JOptionPane.showMessageDialog(null, "This is a test!","THis is a test!", JOptionPane.ERROR_MESSAGE);
        });
    }
}
