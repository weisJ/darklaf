/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package icon;

import java.awt.*;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.color.QuickColorChooser;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.icons.UIAwareIcon;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.event.ThemeInstalledListener;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class IconDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new IconDemo());
    }

    private JPanel iconPanel;

    @Override
    public JComponent createComponent() {
        iconPanel = new JPanel(new GridLayout(3, 2));
        iconPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        UIManager.put("TestIcon.color", new Color(255, 35, 181));

        IconLoader iconLoader = IconLoader.get(IconDemo.class);
        UIAwareIcon uiAwareImageIcon = iconLoader.getUIAwareIcon("aware_image_icon.png");
        UIAwareIcon uiAwareImageIconDual = uiAwareImageIcon.getDual();
        UIAwareIcon uiAwareIcon = iconLoader.getUIAwareIcon("aware_icon.svg");
        UIAwareIcon uiAwareIconDual = uiAwareIcon.getDual();
        Icon themedIcon = iconLoader.getIcon("themed_icon.svg", 16, 16, true);
        Icon imageIcon = iconLoader.getIcon("image_icon.png");

        iconPanel.add(new JLabel("Aware image icon", uiAwareImageIcon, JLabel.LEFT));
        iconPanel.add(new JLabel("Aware image icon (dual)", uiAwareImageIconDual, JLabel.LEFT));
        iconPanel.add(new JLabel("Aware svg icon", uiAwareIcon, JLabel.LEFT));
        iconPanel.add(new JLabel("Aware svg icon (dual)", uiAwareIconDual, JLabel.LEFT));
        iconPanel.add(new JLabel("Themed icon", themedIcon, JLabel.LEFT));
        iconPanel.add(new JLabel("Image icon", imageIcon, JLabel.LEFT));

        DemoPanel panel = new DemoPanel(iconPanel, new BorderLayout(), 0);
        JPanel controlPanel = panel.addControls();
        controlPanel.setLayout(new GridLayout(1, 2));

        controlPanel.add(new JToggleButton("Light/Dark") {
            {
                LafManager.addThemeChangeListener((ThemeInstalledListener) e -> setSelected(Theme.isDark(e.getNewTheme())));
                putClientProperty("JToggleButton.variant", "slider");
                addActionListener(e -> updateAwareIconStyle());
            }
        });
        controlPanel.add(new QuickColorChooser("Themed icon color", UIManager.getColor("TestIcon.color"),
                                               this::updateThemedIconColor));

        return panel;
    }

    private void updateThemedIconColor(final Color c) {
        UIManager.put("TestIcon.color", c);
        IconLoader.updateThemeStatus(LafManager.getInstalledTheme().copy());
        DarkUIUtil.repaint(iconPanel);
    }

    private void updateAwareIconStyle() {
        IconLoader.updateAwareStyle(IconLoader.getAwareStyle().toggle());
        DarkUIUtil.repaint(iconPanel);
    }

    @Override
    public String getTitle() {
        return "Icon Demo";
    }
}
