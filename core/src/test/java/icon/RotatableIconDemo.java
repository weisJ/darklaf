/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package icon;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;
import ui.DemoResources;

import com.github.weisj.darklaf.icons.RotatableIcon;
import com.github.weisj.darklaf.util.Alignment;

public class RotatableIconDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new RotatableIconDemo());
    }

    @Override
    public JComponent createComponent() {
        RotatableIcon folderIcon = new RotatableIcon(DemoResources.FOLDER_ICON);
        folderIcon.setOrientation(Alignment.NORTH);
        JLabel label = new JLabel(folderIcon);
        SwingUtilities.invokeLater(() -> {
            Timer timer = new Timer(1000, e -> {
                folderIcon.setOrientation(folderIcon.getOrientation().clockwise());
                label.repaint();
            });
            timer.setRepeats(true);
            timer.start();
        });
        return new DemoPanel(label);
    }

    @Override
    public String getTitle() {
        return "RotatableIcon Demo";
    }
}
