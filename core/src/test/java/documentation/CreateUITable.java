/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package documentation;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.color.ColorUtil;
import com.github.weisj.darklaf.components.border.DropShadowBorder;
import com.github.weisj.darklaf.icons.DarkSVGIcon;
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.icons.IconColorMapper;
import com.github.weisj.darklaf.icons.StateIcon;
import com.github.weisj.darklaf.parser.ParseResult;
import com.github.weisj.darklaf.parser.Parser;
import com.github.weisj.darklaf.parser.PrimitiveParser;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.ImageUtil;
import com.github.weisj.darklaf.util.StringUtil;
import com.github.weisj.darklaf.util.SystemInfo;
import com.github.weisj.darklaf.util.Types;
import com.kitfox.svg.LinearGradient;
import com.kitfox.svg.SVGDiagram;
import com.kitfox.svg.SVGElement;
import com.kitfox.svg.app.beans.SVGIcon;
import defaults.SampleRenderer;

public class CreateUITable {

    private static final int SAMPLE_WIDTH = 150;
    private static final int SAMPLE_HEIGHT = 25;

    private static final String FOLDER = "documentation/";
    private static final String IDENT = "    ";

    private static final String MISC_GROUP = "__Misc__";
    private static final String THEME_GROUP = "%";

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

        try (OutputStreamWriter writer =
                new OutputStreamWriter(new FileOutputStream(htmlFile), StandardCharsets.UTF_8)) {
            CreateUITable tableCreator = new CreateUITable();
            writer.append("<html>\n");
            writer.append("<a href=\"../index.html\">back</a>\n");
            writer.append(tableCreator.getTableStyle());
            writer.append(tableCreator.createTables(theme, 0));
            writer.append("<a href=\"../index.html\">back</a>\n");
            writer.append("</html>");
        }
    }

    private String getGroup(final Map.Entry<Object, Object> entry) {
        Parser.DebugParseResult value = Types.safeCast(entry.getValue(), Parser.DebugParseResult.class);
        String s = value != null ? value.originalKey : entry.getKey().toString();
        if (s.startsWith(THEME_GROUP)) return THEME_GROUP;
        if (s.contains(".")) return s.split("\\.")[0];
        if (s.endsWith("UI")) return s.substring(0, s.length() - 2);
        return MISC_GROUP + s;
    }

    private String createTables(final Theme theme, final int ident) {
        UIDefaults defaults = setupThemeDefaults(theme);

        Set<String> groups = defaults.entrySet().stream()
                .map(this::getGroup)
                .collect(Collectors.toSet());

        Set<String> miscKeys = groups.stream()
                .filter(s -> s.startsWith(MISC_GROUP))
                .map(s -> s.substring(MISC_GROUP.length()))
                .collect(Collectors.toSet());

        StringBuilder builder = new StringBuilder();
        appendGroup(ident, defaults, builder, THEME_GROUP, "Theme Defaults");
        groups.stream().sorted().filter(s -> !s.startsWith(MISC_GROUP) && !s.equals(THEME_GROUP))
                .forEach(group -> appendGroup(ident, defaults, builder, group, group));
        builder.append(StringUtil.repeat(IDENT, ident)).append("<h3>").append("Miscellaneous").append("</h3>\n");
        appendTable(builder, defaults.entrySet().stream().filter(entry -> miscKeys.contains(entry.getKey().toString()))
                .collect(Collectors.toSet()), ident);

        return builder.toString();
    }

    private UIDefaults setupThemeDefaults(final Theme theme) {
        Parser.setDebugMode(true);
        LafManager.setTheme(theme);
        UIDefaults defaults = new DarkLaf() {
            @Override
            public Theme getTheme() {
                return theme;
            }
        }.getDefaults();
        Parser.setDebugMode(false);
        LafManager.installTheme(theme);
        currentDefaults = UIManager.getLookAndFeelDefaults();
        return defaults;
    }

    private void appendGroup(final int ident, final UIDefaults defaults, final StringBuilder builder,
            final String group, final String heading) {
        builder.append(StringUtil.repeat(IDENT, ident)).append("<h3>").append(heading).append("</h3>\n");
        Set<Map.Entry<Object, Object>> values = defaults.entrySet().stream()
                .filter(entry -> getGroup(entry).equals(group)).collect(Collectors.toSet());
        appendTable(builder, values, ident);
        values.forEach(entry -> defaults.remove(entry.getKey()));
        builder.append('\n');
    }

    private Object unwrap(final Object o) {
        if (o instanceof ParseResult) {
            return ((ParseResult) o).result;
        }
        return o;
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
        values.stream().filter(entry -> entry.getKey().toString().endsWith("UI"))
                .forEach(entry -> appendRow(builder, entry, ident + 1));
        values.stream().filter(entry -> !entry.getKey().toString().endsWith("UI")).sorted((o1, o2) -> {
            Object v1 = unwrap(o1.getValue());
            Object v2 = unwrap(o2.getValue());
            int res = v1.getClass().getSimpleName().compareTo(v2.getClass().getSimpleName());
            if (res != 0) return res;
            return o1.getKey().toString().compareTo(o2.getKey().toString());
        }).forEach(entry -> appendRow(builder, entry, ident + 1));
        builder.append(StringUtil.repeat(IDENT, ident)).append("</table>\n");
    }

    private String getTableStyle() {
        return "<style>\n" + "table {\n" + IDENT + "font-family: monospace;\n" + IDENT + "font-size: 10pt;\n" + IDENT
                + "border-collapse: collapse;\n" + "}\n" + "th, td {\n" + IDENT + "padding: 4px 8px 4px 8px;\n" + "}\n"
                + "tr:nth-child(even) {background-color: #f2f2f2;}\n" + "</style>\n";
    }

    private void appendRow(final StringBuilder builder, final Map.Entry<Object, Object> entry, final int ident) {
        builder.append(StringUtil.repeat(IDENT, ident)).append("<tr>\n");
        String key = entry.getKey().toString();
        if (key.startsWith(THEME_GROUP)) {
            key = key.substring(THEME_GROUP.length());
        }
        appendData(builder, key, ident + 1);
        Object value = entry.getValue();
        if (value instanceof Parser.DebugParseResult
                && ((Parser.DebugParseResult) value).referenceKey != null) {
            Parser.DebugParseResult debugResult = (Parser.DebugParseResult) value;
            appendData(builder, parseValue(debugResult.result), ident + 1); // Value
            appendData(builder, debugResult.referenceKey, ident + 1); // Reference
            builder.append(parsePreview(key, debugResult.result, ident + 1));
        } else {
            value = unwrap(value);
            appendData(builder, parseValue(value), ident + 1); // Value
            appendData(builder, "", ident + 1); // Reference
            builder.append(parsePreview(key, value, ident + 1));
        }
        builder.append(StringUtil.repeat(IDENT, ident)).append("</tr>\n");
    }

    private String parsePreview(final String key, final Object val, final int ident) {
        Object value = getValue(val);
        if (value instanceof StateIcon) {
            value = ((StateIcon) value).getIcon(null);
        }
        if (value instanceof Color) {
            return StringUtil.repeat(IDENT, ident)
                    + String.format("<td style=\"background-color: #%s\" width=\"%d\" height=\"%d\">\n",
                            ColorUtil.toHex((Color) value), SAMPLE_WIDTH, SAMPLE_HEIGHT);
        } else if (value instanceof DarkSVGIcon) {
            return parseSVGIcon((DarkSVGIcon) value, ident);
        } else if ((value instanceof Border && !(value instanceof EmptyBorder)) || value instanceof Font
                || (value instanceof Icon && !(value instanceof EmptyIcon))) {
            return parseImage(key, value, ident);
        }
        return StringUtil.repeat(IDENT, ident) + "<td></td>\n";
    }

    private Object getValue(final Object val) {
        Object value = val;
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
        } catch (final IOException ignored) {
            return StringUtil.repeat(IDENT, ident) + "<td></td>\n";
        }
        return StringUtil.repeat(IDENT, ident) + String
                .format("<td style=\"padding:0\" align=\"center\"><img src=\"%s\" alt=\"%s\"></td>\n", path, key);
    }

    private String createImage(final Object value, final String name, final Dimension size) throws IOException {
        // noinspection ResultOfMethodCallIgnored
        new File(workingFolder + "img/").mkdirs();
        String fileName = "img/" + name + "_" + SystemInfo.getOsName() + ".png";
        File imageFile = new File(workingFolder + fileName);
        if (!imageFile.createNewFile()) return fileName;
        if (value instanceof Icon) {
            size.width = Math.max(size.width, ((Icon) value).getIconWidth());
            size.height = Math.max(size.height, ((Icon) value).getIconHeight());
        }
        JComponent comp =
                (JComponent) new SampleRenderer().getTableCellRendererComponent(null, value, false, false, 0, 0);
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
        } catch (final IOException e) {
            e.printStackTrace();
        }
        sb.append(StringUtil.repeat(IDENT, ident)).append("</td>\n");

        String svg = sb.toString();

        SVGDiagram svgDiagram = icon.getSvgUniverse().getDiagram(icon.getSvgURI());
        SVGElement defs = svgDiagram.getElement("colors");
        if (defs != null) {
            List<?> children = defs.getChildren(null);
            for (Object child : children) {
                if (child instanceof LinearGradient) {
                    float opacity = IconColorMapper.getOpacity((LinearGradient) child, currentDefaults, null);
                    if (opacity < 0) opacity = 1;
                    Color color = IconColorMapper.getColor((LinearGradient) child, currentDefaults, null);
                    String id = ((LinearGradient) child).getId();
                    String match = "=\"url\\(#" + id + "\\)\"";
                    String fillReplacement = "fill=\"#" + ColorUtil.toHex(color) + "\"";
                    if (opacity != 1) {
                        fillReplacement += " fill-opacity=\"" + opacity + "\"";
                        svg = svg.replaceAll("fill-opacity=\"[^\"]*\"", "");
                    }
                    svg = svg.replaceAll("fill" + match, fillReplacement);

                    String strokeReplacement = "stroke=\"#" + ColorUtil.toHex(color) + "\"";
                    if (opacity != 1) strokeReplacement += " stroke-opacity=\"" + opacity + "\"";
                    svg = svg.replaceAll("stroke" + match, strokeReplacement);
                }
            }
            svg = svg.replaceAll("<defs id=\"colors\">(\\n.*)* </defs>\\s+", "");
        }
        return svg;
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
            return String.format("%s%s [%03d,%03d,%03d]",
                    PrimitiveParser.COLOR_PREFIX, ColorUtil.toHex(color),
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
