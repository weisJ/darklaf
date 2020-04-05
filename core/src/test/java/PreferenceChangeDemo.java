import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.Theme;
import ui.ComponentDemo;

import javax.swing.*;
import java.awt.*;

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
public class PreferenceChangeDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new PreferenceChangeDemo());
    }

    @Override
    public JComponent createComponent() {
        LafManager.addThemePreferenceChangeListener(LafManager::installTheme);
        return new JPanel(new GridBagLayout()) {{
            add(new JToggleButton("Start") {{
                addActionListener(e -> {
                    setText(isSelected() ? "Stop" : "Start");
                    LafManager.enabledPreferenceChangeReporting(isSelected());
                });
            }});
        }};
    }

    @Override
    public String getTitle() {
        return "Preference Change Demo";
    }

    @Override
    public Theme createTheme() {
        return LafManager.themeForPreferredStyle(LafManager.getPreferredThemeStyle());
    }

    @Override
    public JMenuBar createMenuBar() {
        return null;
    }
}
