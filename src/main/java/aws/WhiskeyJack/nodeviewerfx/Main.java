/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import aws.WhiskeyJack.QandA.*;
import java.io.*;
import javafx.application.*;
import javafx.fxml.*;
import javafx.scene.*;
import javafx.stage.*;

public class Main extends Application {
    @Override
    public void start(Stage primaryStage) {
        try {
            primaryStage.setTitle("Node Editor Test");
            primaryStage.setScene(new Scene(new FXMLLoader(GraphView.class.getResource("NodeEditor.fxml")).load(), 1000, 640));
            primaryStage.show();
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
        }
    }
    public static void main(String[] args) {
        QuestionsByTag.dump();
        Question.extract(q->true).forEach(q->System.out.println(q.get("tag", "yuk")));
        launch(args);
    }
}
