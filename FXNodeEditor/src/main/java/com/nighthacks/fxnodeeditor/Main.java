/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor;

import java.io.IOException;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class Main extends Application {
    
    @Override
    public void start(Stage primaryStage) {
        try {
            primaryStage.setTitle("Node Editor Test");
            primaryStage.setScene(new Scene(new FXMLLoader(Main.class.getResource("nodeEditor.fxml")).load(), 640, 480));
            primaryStage.show();
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        launch(args);
    }
    
}
