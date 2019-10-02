import com.weis.darklaf.LafManager;

import javax.swing.*;
import java.awt.*;

public class DarklafDemo {

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.loadLaf(LafManager.Theme.Dark);

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
