/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.metaedit;

import com.aws.jag.fxnodeeditor.util.*;
import com.aws.jag.fxnodeeditor.view.*;
import com.aws.jag.fxnodeeditor.gengraph.*;
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
            var root = (Parent)new FXMLLoader(MetaEditorController.class.getResource("MetaEditor.fxml")).load();
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
    private ListView<MetaPort> meParamList;
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
        meType.getItems().addAll(Type.allTypes());
        meParamList.getSelectionModel().selectedItemProperty()
                .addListener(e -> selectParam(meParamList.getSelectionModel().getSelectedItem()));
        populate(first);
    }
    @FXML
    public void doSave(ActionEvent event) {
        System.out.println("doSave");
    }
    @FXML
    public void meDelete(ActionEvent event) {
        System.out.println("meDelete");
    }
    private void populate(MetaNode mn) {
        if(mn==current) return;
        current = mn;
        selectParam(null);
        if(mn != null) {
            current = mn;
            meName.setText(mn.getName());
            meDescription.setText(mn.getDescription());
            var items = meParamList.getItems();
            items.clear();
            mn.forEachPort(p -> items.add((MetaPort)p));
        }
    }
    private MetaPort currentPort;
    private void selectParam(MetaPort ae) {
        if(currentPort == ae) return;
        if(currentPort!=null) {
            var name = meParameterName.getText();
            if(!Objects.equals(name, currentPort.getName())) {
                currentPort.setName(name);
                nec.adjustNames();
                currentPort.markDirty();
            }
            var desc = meParamDesc.getText();
            if(!Objects.equals(desc, currentPort.description)) {
                currentPort.description = desc;
                currentPort.markDirty();
            }
            var value = meValue.getText();
            if(!Objects.equals(value, currentPort.defaultValue)) {
                currentPort.defaultValue = value;
                currentPort.markDirty();
            }
            var type = meType.getValue();
            if(!Objects.equals(type, currentPort.type)) {
                currentPort.type = Type.of(type);
                currentPort.markDirty();
            }
        }
        if(ae != null) {
            meIsInput.setSelected(ae.in);
            meParameterName.setText(ae.getName());
            meValue.setText(Coerce.toString(ae.defaultValue));
            meParamDesc.setText(ae.description);
            meType.selectionModelProperty().get().select(ae.type.getName());
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
