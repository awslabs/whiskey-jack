/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.metaedit;

import com.aws.jag.fxnodeeditor.graph.*;
import com.aws.jag.fxnodeeditor.meta.*;
import com.aws.jag.fxnodeeditor.util.*;
import java.net.*;
import java.util.*;
import javafx.event.*;
import javafx.fxml.*;
import javafx.scene.*;
import javafx.scene.control.*;
import javafx.stage.*;

public class MetaEditorController implements Initializable {
    public static void edit(MetaNode m, NodeEditorController parent) {
        nec = parent;
        if(singleton != null)
            singleton.populate(m);
        else {
            first = m;
            launch();
        }
        stage.show();
    }
    public static void launch() {
        if(singleton == null) try {
            Parent root = new FXMLLoader(MetaEditorController.class.getResource("MetaEditor.fxml")).load();
            var scene = new Scene(root, 600, 800);
            stage = new Stage();
            stage.setTitle("Product catalog entry editor");
            stage.setScene(scene);
        } catch(Throwable ex) {
            Dlg.error("Launching meta editor dialog", ex);
        }
    }
    private static NodeEditorController nec;
    private MetaNode current;
    private static MetaNode first;
    private static MetaEditorController singleton;
    private static Stage stage;
    @FXML
    private TextArea meDescription;
    @FXML
    private ComboBox<String> meDomain;
    @FXML
    private CheckBox meIsInput;
    @FXML
    private TextField meName;
    @FXML
    private TextArea meParamDesc;
    @FXML
    private ListView<Port> meParamList;
    @FXML
    private TextField meParameterName;
    @FXML
    private ComboBox<String> meType;
    @FXML
    private TextField meValue;
    {
        singleton = this;
    }
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        System.out.println("Meta edit");
        meDomain.getItems().addAll("Anywhere", "Device", "AWS", "Network", "With neighbor");
        meType.getItems().addAll("int", "float", "boolean", "String");
        meParamList.getSelectionModel().selectedItemProperty().addListener(e -> selectParam(meParamList.getSelectionModel().getSelectedItem()));
        populate(first);
    }
    @FXML
    void doSave(ActionEvent event) {
        System.out.println("doSave");
    }
    @FXML
    void meDelete(ActionEvent event) {
        System.out.println("meDelete");
    }
    private void populate(MetaNode mn) {
        if(mn==current) return;
        current = mn;
        selectParam(null);
        if(mn != null) {
            current = mn;
            meName.setText(mn.name);
            meDescription.setText(mn.description);
            var items = meParamList.getItems();
            items.clear();
            mn.forAllChildren(p -> items.add(p));
        }
    }
    private Port currentPort;
    private void selectParam(Port ae) {
        if(currentPort == ae) return;
        if(currentPort!=null) {
            var name = meParameterName.getText();
            if(!Objects.equals(name, currentPort.name)) {
                currentPort.name = name;
                nec.adjustNames();
                currentPort.setDirty();
            }
            var desc = meParamDesc.getText();
            if(!Objects.equals(desc, currentPort.description)) {
                currentPort.description = desc;
                currentPort.setDirty();
            }
            var value = meValue.getText();
            if(!Objects.equals(value, currentPort.dflt)) {
                currentPort.dflt = value;
                currentPort.setDirty();
            }
            var type = meType.getValue();
            if(!Objects.equals(type, currentPort.type)) {
                currentPort.type = type;
                currentPort.setDirty();
            }
        }
        if(ae != null) {
            meIsInput.setSelected(ae.in);
            meParameterName.setText(ae.name);
            meValue.setText(Coerce.toString(ae.dflt));
            meParamDesc.setText(ae.description);
            meType.selectionModelProperty().get().select(ae.type);
            currentPort = ae;
        } else {
            meIsInput.setSelected(false);
            meParameterName.setText("");
            meValue.setText("");
            meParamDesc.setText("");
            meType.selectionModelProperty().get().select("");
            currentPort = null;
        }
    }
}
