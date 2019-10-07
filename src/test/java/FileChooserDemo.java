import com.weis.darklaf.LafManager;
import com.weis.darklaf.theme.Theme;

import javax.swing.*;

public final class FileChooserDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            var chooser = new JFileChooser(System.getProperty("user.home"));
            var frame = new JFrame();
            frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            frame.setSize(100, 100);
            frame.setVisible(true);
            frame.setLocationRelativeTo(null);
            chooser.showOpenDialog(frame);
        });
    }
}
