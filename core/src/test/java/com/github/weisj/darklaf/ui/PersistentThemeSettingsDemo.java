/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui;

import java.io.*;
import java.util.prefs.Preferences;

import javax.swing.*;

import com.github.weisj.darklaf.settings.SettingsConfiguration;
import com.github.weisj.darklaf.settings.ThemeSettings;

public class PersistentThemeSettingsDemo {

    public static void main(final String[] args) {
        load();
        Runtime.getRuntime().addShutdownHook(new Thread(PersistentThemeSettingsDemo::save));
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Persistent Theme Demo");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setContentPane(new SettingsDemo().createComponent());
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }

    private static void load() {
        ThemeSettings settings = ThemeSettings.getInstance();
        Preferences preferences = Preferences.userNodeForPackage(PersistentThemeSettingsDemo.class);
        byte[] serializedSettings = preferences.getByteArray("themeSettings", null);
        if (serializedSettings != null) {
            try (ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(serializedSettings))) {
                SettingsConfiguration config = (SettingsConfiguration) in.readObject();
                settings.setConfiguration(config);
            } catch (IOException | ClassNotFoundException e) {
                e.printStackTrace();
            }
        }
        settings.apply();
    }

    private static void save() {
        ThemeSettings settings = ThemeSettings.getInstance();
        SettingsConfiguration config = settings.exportConfiguration();
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try (ObjectOutputStream out = new ObjectOutputStream(bos)) {
            out.writeObject(config);
            out.flush();
            Preferences preferences = Preferences.userNodeForPackage(PersistentThemeSettingsDemo.class);
            preferences.putByteArray("themeSettings", bos.toByteArray());
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

}
