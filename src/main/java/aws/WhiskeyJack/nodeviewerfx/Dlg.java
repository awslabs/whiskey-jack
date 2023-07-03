/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import static aws.WhiskeyJack.util.Utils.*;
import java.lang.reflect.*;
import java.util.*;
import javafx.scene.*;
import javafx.scene.control.*;
import static javafx.scene.control.ButtonType.*;
import javafx.scene.image.*;
import javafx.scene.layout.*;

public class Dlg {
    private static final int maxStringWidth = 60;
    private String title;
    private final VBox body = new VBox();
    private VBox expandable;
    private ButtonType auxButton;
    public static void error(Object... o) {
        new Dlg().show(errorIcon, "error", o);
    }
    public static void note(Object... o) {
        new Dlg().show(noteIcon, "note", o);
    }
    public static String ask(String dflt, Object... o) {
        var t = new TextField(dflt);
        var r = new Dlg().show(questionIcon, "note", t, o);
        System.out.println("Returns " + r);
        return r == ButtonType.OK ? t.getText() : null;
    }
    private Dlg() {
    }
    private Object show(Image icon, String style, Object... message) {
        var d = new Dialog();
        d.initOwner(GraphView.rootWindow());
        body.getStyleClass().clear();
        d.setGraphic(img(icon));
        body.getStyleClass().add(style + "Dialog");
        var dp = d.getDialogPane();
        dp.getStyleClass().add(style);
        dp.setContent(body);
        addStuff(message, "dlgTitle");
        dp.setExpandableContent(expandable);
        d.setTitle(title == null ? "Error" : title);
        var buttons = dp.getButtonTypes();
        if(!buttons.contains(ButtonType.OK)) buttons.add(ButtonType.OK);
        if(auxButton != null && !buttons.contains(auxButton))
            buttons.add(auxButton);
        dp.getStylesheets().add(Dlg.class.getResource("error.css").toExternalForm());
        dp.setMinWidth(Region.USE_COMPUTED_SIZE);
        dp.setPrefWidth(Region.USE_COMPUTED_SIZE);
        dp.setMaxWidth(1000);
        d.setResizable(true);
        var opt = d.showAndWait();
        return opt.isPresent() ? opt.get() : null;
    }
    private ImageView img(Image i) {
        var img = new ImageView(i);
        img.setPreserveRatio(true);
        img.setFitHeight(64);
        return img;
    }
    private String addStuff(Object o, String style) {
        if(o == null)
            return style;
        if(o.getClass().isArray()) {
            var len = Array.getLength(o);
            for(var i = 0; i < len; i++)
                style = addStuff(Array.get(o, i), style);
        } else
            switch(o) {
                case null -> {
                }
                case Node n -> {
                    body.getChildren().add(n);
                    if(n instanceof Control) auxButton = CANCEL;
                }
                case Throwable t -> {
                    t.printStackTrace(System.out); // TODO eliminate
                    System.out.flush();
                    var u = getUltimateCause(t);
                    var m = u.getMessage();
                    if(isEmpty(m))
                        m = u.toString();
                    addStuff(m, style);
                    style = "dlgerrTitle";
                    if(expandable == null)
                        expandable = new VBox();
                    for(var e: u.getStackTrace())
                        addString(expandable, e.getMethodName() + '(' + e.getFileName() + ':' + e.getLineNumber() + ')', "dlgerrBody");
                }
                case Collection c ->
                    c.forEach(e -> addStuff(e, "dlgBody"));
                default -> {
                    addString(body, deepToString(o, maxStringWidth).toString(), style);
                    style = "dlgBody";
                }
            }
        return style;
    }
    private void addString(VBox dest, String s, String style) {
        var v = dest.getChildren();
        if(!isEmpty(s))
            if(title == null)
                title = s;
            else if(v.size() < 80) {
                var l = new Label(s);
                l.setWrapText(true);
                l.setPrefWidth(maxStringWidth * 10);
                l.getStyleClass().add(style);
                v.add(l);
            }
    }
    static final private Image errorIcon = new Image(NodeView.class.getResourceAsStream("Oops.png"));
    static final private Image noteIcon = new Image(NodeView.class.getResourceAsStream("OK.png"));
    static final private Image questionIcon = new Image(NodeView.class.getResourceAsStream("QuestionBubble.png"));

}
