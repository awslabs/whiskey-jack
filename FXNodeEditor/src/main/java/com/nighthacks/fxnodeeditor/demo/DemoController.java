/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.demo;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Label;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;

/**
 * FXML Controller class
 *
 * @author jag
 */
public class DemoController implements Initializable {
    @FXML private Label statusText;
    @FXML private WebView web;
    @FXML private AnchorPane nodeEditor;
    private int strike;
    @FXML
    void Yo() throws IOException {
        System.out.println("Hello! "+statusText);
        statusText.setText("String " + ++strike);
        web.getEngine().load("http://slashdot.org");
    }

    @Override
    public void initialize(URL url, ResourceBundle rb) {
        // TODO
    }    
    
}
