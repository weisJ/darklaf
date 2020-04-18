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
import java.awt.*;

import javax.swing.*;

import ui.ComponentDemo;

import com.github.weisj.darklaf.components.DefaultButton;
import com.github.weisj.darklaf.settings.ThemeSettings;
import com.github.weisj.darklaf.util.ImageUtil;

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
 */
public class SettingsDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new SettingsDemo());
    }

    @Override
    public JComponent createComponent() {
        JPanel content = new JPanel(new BorderLayout());
        ThemeSettings settings = ThemeSettings.getInstance();
        JComponent settingsPanel = settings.getSettingsPanel();
        content.add(settingsPanel, BorderLayout.CENTER);
        Box box = Box.createHorizontalBox();
        box.setBorder(settingsPanel.getBorder());
        box.add(Box.createHorizontalGlue());
        box.add(new DefaultButton("Apply") {
            {
                addActionListener(e -> settings.apply());
            }
        });
        box.add(new JButton("Revert") {
            {
                addActionListener(e -> settings.revert());
            }
        });
        content.add(box, BorderLayout.SOUTH);
        return content;
    }

    @Override
    public Image getIconImage() {
        return ImageUtil.createFrameIcon(ThemeSettings.getInstance().getIcon());
    }

    @Override
    public JMenuBar createMenuBar() {
        return null;
    }

    @Override
    public String getTitle() {
        return "Settings Demo";
    }
}
