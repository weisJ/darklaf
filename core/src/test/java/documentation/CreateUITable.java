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
package documentation;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.components.border.DropShadowBorder;
import com.github.weisj.darklaf.icons.DarkSVGIcon;
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.*;
import com.github.weisj.darklaf.util.ImageUtil;
import com.kitfox.svg.app.beans.SVGIcon;
import defaults.SampleRenderer;

public class CreateUITable {

    private static final int SAMPLE_WIDTH = 150;
    private static final int SAMPLE_HEIGHT = 25;

    private static final String FOLDER = "documentation/";
    private static final String IDENT = "    ";

    private static String workingFolder;
    private UIDefaults currentDefaults;

    public static void main(final String[] args) throws IOException {
        for (Theme theme : LafManager.getRegisteredThemes()) {
            createThemeDefaultsPage(theme);
        }
    }

    public static void createThemeDefaultsPage(final Theme theme) throws IOException {
        workingFolder = FOLDER + theme.getPrefix() + "/";
        String os = SystemInfo.getOsName();
        String htmlFile = workingFolder + "defaults_" + os + ".html";
        Files.createDirectories(new File(workingFolder).toPath());
        File f = new File(htmlFile);
        if (!f.exists()) Files.createFile(f.toPath());

        try (OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(htmlFile),
                                                                StandardCharsets.UTF_8)) {
            CreateUITable tableCreator = new CreateUITable();
            writer.append("<html>\n");
            writer.append("<a href=\"../index.html\">back</a>\n");
            writer.append(tableCreator.getTableStyle());
            writer.append(tableCreator.createTables(theme, 0));
            writer.append("<a href=\"../index.html\">back</a>\n");
            writer.append("</html>");
        }
    }

    private String createTables(final Theme theme, final int ident) {
        UIDefaults defaults = setupThemeDefaults(theme);

        String misc = "__Misc__";

        Set<String> groups = defaults.keySet().stream().map(key -> {
            String s = key.toString();
            if (s.startsWith("%")) return "%";
            if (s.contains(".")) return s.split("\\.")[0];
            if (s.endsWith("UI")) return s.substring(0, s.length() - 2);
            return misc + s;
        }).collect(Collectors.toSet());

        Set<String> miscKeys = groups.stream().filter(s -> s.startsWith(misc))
                                     .map(s -> s.substring(misc.length())).collect(Collectors.toSet());

        StringBuilder builder = new StringBuilder();
        appendGroup(ident, defaults, builder, "%", "Theme Defaults");
        groups.stream().sorted()
              .filter(s -> !s.startsWith(misc) && !s.equals("%"))
              .forEach(group -> appendGroup(ident, defaults, builder, group, group));
        builder.append(StringUtil.repeat(IDENT, ident)).append("<h3>").append("Miscellaneous").append("</h3>\n");
        appendTable(builder, defaults.entrySet().stream()
                                     .filter(entry -> miscKeys.contains(entry.getKey().toString()))
                                     .collect(Collectors.toSet()),
                    ident);

        return builder.toString();
    }

    private UIDefaults setupThemeDefaults(final Theme theme) {
        PropertyLoader.setAddReferenceInfo(true);
        UIDefaults defaults = new DarkLaf().getDefaults();
        PropertyLoader.setAddReferenceInfo(false);
        LafManager.installTheme(theme);
        currentDefaults = UIManager.getLookAndFeelDefaults();
        return defaults;
    }

    private void appendGroup(final int ident, final UIDefaults defaults, final StringBuilder builder,
                             final String group, final String heading) {
        builder.append(StringUtil.repeat(IDENT, ident)).append("<h3>").append(heading).append("</h3>\n");
        Set<Map.Entry<Object, Object>> values = defaults.entrySet().stream()
                                                        .filter(entry -> {
                                                            String key = entry.getKey().toString();
                                                            if (key.startsWith("%")) return true;
                                                            if (key.endsWith("UI"))
                                                                return key.substring(0, key.length() - 2).equals(group);
                                                            if (key.contains("."))
                                                                return key.split("\\.")[0].equals(group);
                                                            return key.equals(group);
                                                        })
                                                        .collect(Collectors.toSet());
        appendTable(builder, values, ident);
        values.forEach(entry -> defaults.remove(entry.getKey()));
        builder.append('\n');
    }

    private void appendTable(final StringBuilder builder, final Set<Map.Entry<Object, Object>> values,
                             final int ident) {
        builder.append(StringUtil.repeat(IDENT, ident)).append("<table>\n");
        builder.append(StringUtil.repeat(IDENT, ident + 1)).append("<tr>\n");
        builder.append(StringUtil.repeat(IDENT, ident + 2)).append("<th>Key</th>\n");
        builder.append(StringUtil.repeat(IDENT, ident + 2)).append("<th>Value</th>\n");
        builder.append(StringUtil.repeat(IDENT, ident + 2)).append("<th>Reference</th>\n");
        builder.append(StringUtil.repeat(IDENT, ident + 2)).append("<th>Preview</th>\n");
        builder.append(StringUtil.repeat(IDENT, ident + 1)).append("</tr>\n");
        values.stream()
              .filter(entry -> entry.getKey().toString().endsWith("UI"))
              .forEach(entry -> appendRow(builder, entry, ident + 1));
        values.stream()
              .filter(entry -> !entry.getKey().toString().endsWith("UI"))
              .sorted((o1, o2) -> {
                  int res = o1.getValue().getClass().getSimpleName()
                              .compareTo(o2.getValue().getClass().getSimpleName());
                  if (res != 0) return res;
                  Object val1 = o1.getValue();
                  Object val2 = o2.getValue();
                  if (val1 instanceof Comparable) {
                      return ((Comparable<Object>) val1).compareTo(val2);
                  } else {
                      return val1.toString().compareTo(val2.toString());
                  }
              })
              .forEach(entry -> appendRow(builder, entry, ident + 1));
        builder.append(StringUtil.repeat(IDENT, ident)).append("</table>\n");
    }

    private String getTableStyle() {
        return "<style>\n"
               + "table {\n"
               + IDENT + "font-family: monospace;\n"
               + IDENT + "font-size: 10pt;\n"
               + IDENT + "border-collapse: collapse;\n"
               + "}\n"
               + "th, td {\n"
               + IDENT + "padding: 4px 8px 4px 8px;\n"
               + "}\n"
               + "tr:nth-child(even) {background-color: #f2f2f2;}\n"
               + "</style>\n";
    }

    private void appendRow(final StringBuilder builder,
                           final Map.Entry<Object, Object> entry,
                           final int ident) {
        builder.append(StringUtil.repeat(IDENT, ident)).append("<tr>\n");
        String key = entry.getKey().toString();
        appendData(builder, key, ident + 1);
        Object value = entry.getValue();
        if (value instanceof PropertyLoader.ReferenceInfo) {
            PropertyLoader.ReferenceInfo<?> info = (PropertyLoader.ReferenceInfo<?>) value;
            appendData(builder, parseValue(info.getValue()), ident + 1); // Value
            appendData(builder, info.getReferenceKey(), ident + 1); // Reference
            builder.append(parsePreview(key, info.getValue(), ident + 1));
        } else {
            appendData(builder, parseValue(value), ident + 1); // Value
            appendData(builder, "", ident + 1); // Reference
            builder.append(parsePreview(key, value, ident + 1));
        }
        builder.append(StringUtil.repeat(IDENT, ident)).append("</tr>\n");
    }

    private String parsePreview(final String key, final Object val, final int ident) {
        Object value = getValue(val);
        if (value instanceof Color) {
            return StringUtil.repeat(IDENT, ident)
                   + String.format("<td style=\"background-color: #%s\" width=\"%d\" height=\"%d\">\n",
                                   ColorUtil.toHex((Color) value), SAMPLE_WIDTH, SAMPLE_HEIGHT);
        } else if (value instanceof DarkSVGIcon) {
            return parseSVGIcon((DarkSVGIcon) value, ident);
        } else if ((value instanceof Border && !(value instanceof EmptyBorder))
                   || value instanceof Font
                   || (value instanceof Icon && !(value instanceof EmptyIcon))) {
            return parseImage(key, value, ident);
        }
        return StringUtil.repeat(IDENT, ident) + "<td></td>\n";
    }

    private Object getValue(final Object val) {
        Object value = PropertyLoader.unpackReference(val);
        if (value instanceof UIDefaults.ActiveValue) {
            value = ((UIDefaults.ActiveValue) value).createValue(currentDefaults);
        }
        if (value instanceof UIDefaults.LazyValue) {
            value = ((UIDefaults.LazyValue) value).createValue(currentDefaults);
        }
        return value;
    }

    private String parseImage(final String key, final Object value, final int ident) {
        String stringRepresentation = parseValue(value).replaceAll(" ", "").replaceAll("_", "");
        String keyName = key.replace(" ", "").replaceAll("_", "");
        String path;
        Dimension size = new Dimension(SAMPLE_WIDTH, SAMPLE_HEIGHT);
        try {
            if (!(value instanceof Icon)) {
                path = createImage(value, stringRepresentation, size);
            } else {
                path = createImage(value, keyName, size);
            }
        } catch (IOException ignored) {
            return StringUtil.repeat(IDENT, ident) + "<td></td>\n";
        }
        return StringUtil.repeat(IDENT, ident)
               + String.format("<td style=\"padding:0px\" align=\"center\"><img src=\"%s\" alt=\"%s\"></td>\n", path,
                               key);
    }

    private String createImage(final Object value, final String name, final Dimension size) throws IOException {
        new File(workingFolder + "img/").mkdirs();
        String fileName = "img/" + name + ".png";
        File imageFile = new File(workingFolder + fileName);
        if (!imageFile.createNewFile()) return fileName;
        if (value instanceof Icon) {
            size.width = Math.max(size.width, ((Icon) value).getIconWidth());
            size.height = Math.max(size.height, ((Icon) value).getIconHeight());
        }
        JComponent comp = (JComponent) new SampleRenderer().getTableCellRendererComponent(null, value, false, false, 0,
                                                                                          0);
        BufferedImage image = ImageUtil.createCompatibleTranslucentImage(size.width, size.height);
        Graphics g = image.getGraphics();
        if (!(value instanceof Icon) && !(value instanceof DropShadowBorder)) {
            g.setColor(new JPanel().getBackground());
            g.fillRect(0, 0, size.width, size.height);
        }
        comp.setBounds(0, 0, size.width, size.height);
        comp.setOpaque(false);
        comp.paint(g);
        g.dispose();
        ImageIO.write(image, "png", imageFile);
        return fileName;
    }

    private String parseSVGIcon(final DarkSVGIcon value, final int ident) {
        SVGIcon icon = value.getSVGIcon();
        StringBuilder sb = new StringBuilder(StringUtil.repeat(IDENT, ident)).append("<td align=\"center\">\n");
        try {
            readFile(icon.getSvgURI().toURL(), sb, ident + 1);
        } catch (IOException e) {
            e.printStackTrace();
        }
        sb.append(StringUtil.repeat(IDENT, ident)).append("</td>\n");
        String svg = sb.toString();
        Matcher matcher = Pattern.compile("url\\(#([^()]*)\\)").matcher(svg);
        StringBuffer result = new StringBuffer();
        while (matcher.find()) {
            String color = String.format("#%s", ColorUtil.toHex(getColor(currentDefaults, matcher.group(1))));
            matcher.appendReplacement(result, color);
        }
        matcher.appendTail(result);
        return result.toString();
    }

    private Color getColor(final UIDefaults defaults, final String key) {
        Object obj = getValue(defaults.get(key));
        if (obj instanceof Color) return (Color) obj;
        return null;
    }

    private void readFile(final URL url, final StringBuilder builder, final int ident) throws IOException {
        InputStream inputStream = url.openStream();
        InputStreamReader isReader = new InputStreamReader(inputStream);
        // Creating a BufferedReader object
        BufferedReader reader = new BufferedReader(isReader);
        String str;
        while ((str = reader.readLine()) != null) {
            builder.append(StringUtil.repeat(IDENT, ident)).append(str).append('\n');
        }
    }

    private void appendData(final StringBuilder builder, final Object value, final int ident) {
        builder.append(StringUtil.repeat(IDENT, ident)).append("<td>");
        builder.append(value);
        builder.append("</td>\n");
    }

    private String parseValue(final Object val) {
        Object value = getValue(val);
        if (value instanceof Color) {
            Color color = (Color) value;
            return String.format("#%s [%03d,%03d,%03d]",
                                 ColorUtil.toHex(color),
                                 color.getRed(), color.getGreen(), color.getBlue());
        } else if (value instanceof Insets) {
            Insets insets = (Insets) value;
            return String.format("Insets [%d,%d,%d,%d]", insets.top, insets.left, insets.bottom, insets.right);
        } else if (value instanceof Dimension) {
            Dimension dim = (Dimension) value;
            return String.format("Dimension [%d,%d]", dim.width, dim.height);
        } else if (value instanceof Icon) {
            Icon icon = (Icon) value;
            return String.format("Icon [%d,%d]", icon.getIconWidth(), icon.getIconHeight());
        } else if (value instanceof Font) {
            String font = value.toString();
            return String.format("Font %s", font.substring(font.indexOf('[')));
        } else if (value instanceof CharSequence) {
            return value.toString();
        } else if (value instanceof Number) {
            return value.toString();
        } else if (value instanceof Boolean) {
            return value.toString();
        } else if (value instanceof Character) {
            return value.toString();
        }
        if (value == null) return "null";
        return value.getClass().getSimpleName();
    }
}
