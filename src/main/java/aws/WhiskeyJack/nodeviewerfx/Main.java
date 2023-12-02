/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import aws.WhiskeyJack.QandA.*;
import java.io.*;
import java.util.prefs.Preferences;
import java.util.function.*;
import javafx.application.*;
import javafx.beans.property.*;
import javafx.fxml.*;
import javafx.scene.*;
import javafx.scene.image.*;
import javafx.stage.*;

public class Main extends Application {
    private static final Preferences prefs = Preferences.userNodeForPackage(Main.class);
    @Override
    public void start(Stage win) {
        try {
            setPref("w", win.widthProperty(), w -> win.setWidth(w));
            setPref("h", win.heightProperty(), w -> win.setHeight(w));
            setPref("x", win.xProperty(), w -> win.setX(w));
            setPref("y", win.yProperty(), w -> win.setY(w));
            win.setTitle("Whiskey Jack");
            win.setScene(new Scene(new FXMLLoader(GraphView.class.getResource("NodeEditor.fxml")).load(), 1000, 640));
            win.show();
            Platform.runLater(() -> {
                var gv = GraphView.rootWindow();
                if(gv != null) gv.requestFocus();
                win.getIcons().add(new Image(NodeView.class
                        .getResourceAsStream("WhiskeyJackIcon.png")));
            });
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
        }
    }
    private void setPref(String name, ReadOnlyDoubleProperty getter, DoubleConsumer setter) {
        var v = prefs.getDouble(name, -42);
        if(v >= 0) setter.accept(v);
        getter.addListener((e, o, n) -> prefs.putDouble(name, n.doubleValue()));
    }
    public static void main(String[] args) {
        QuestionsByTag.dump();
        Question.extract(q -> true).forEach(q ->
                System.out.println(q.get("tag", "yuk")));
        launch(args);
    }
}
