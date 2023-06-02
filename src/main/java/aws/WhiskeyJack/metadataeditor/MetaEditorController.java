/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.metadataeditor;

import aws.WhiskeyJack.nodeviewerfx.Dlg;
import aws.WhiskeyJack.nodeviewerfx.GraphView;
import aws.WhiskeyJack.nodegraph.MetaNode;
import aws.WhiskeyJack.nodegraph.MetaPort;
import aws.WhiskeyJack.nodegraph.Type;
import aws.WhiskeyJack.nodegraph.Domain;
import aws.WhiskeyJack.util.Coerce;
import java.io.*;
import java.net.*;
import java.util.*;
import javafx.event.*;
import javafx.fxml.*;
import javafx.scene.*;
import javafx.scene.control.*;
import javafx.stage.*;

public class MetaEditorController implements Initializable {
    public static void edit(MetaNode m, GraphView parent) {
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
            var root = (Parent) new FXMLLoader(MetaEditorController.class.getResource("MetaEditor.fxml")).load();
            var scene = new Scene(root, 600, 800);
            stage = new Stage();
            stage.setTitle("Product catalog entry editor");
            stage.setScene(scene);
        } catch(IOException | Error ex) {
            Dlg.error("Launching meta editor dialog", ex);
        }
    }
    private static GraphView nec;
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
        for(var d: Domain.allInteresting())
            meDomain.getItems().add(d.getName());
        for(var t: Type.allInteresting())
            meType.getItems().add(t.getName());
        meParamList.getSelectionModel().selectedItemProperty()
                .addListener(e ->
                        selectParam(meParamList.getSelectionModel().getSelectedItem()));
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
        if(mn == current) return;
        current = mn;
        selectParam(null);
        if(mn != null) {
            current = mn;
            meName.setText(mn.getName());
            meDescription.setText(mn.getDescription());
            var items = meParamList.getItems();
            items.clear();
            mn.forEachPort(p -> items.add((MetaPort) p));
        }
    }
    private MetaPort currentPort;
    private void selectParam(MetaPort ae) {
        if(currentPort == ae) return;
        if(currentPort != null) {
            var name = meParameterName.getText();
            if(!Objects.equals(name, currentPort.getName())) {
                currentPort.setName(name);
                nec.adjustNames();
                currentPort.markDirty();
            }
            var desc = meParamDesc.getText();
            if(!Objects.equals(desc, currentPort.getDescription())) {
                currentPort.setDescription(desc);
                currentPort.markDirty();
            }
            var value = meValue.getText();
            if(!Objects.equals(value, currentPort.getValue())) {
                currentPort.setValue(value);
                currentPort.markDirty();
            }
            var type = meType.getValue();
            if(!Objects.equals(type, currentPort.getType())) {
                currentPort.setType(Type.of(type));
                currentPort.markDirty();
            }
        }
        if(ae != null) {
            meIsInput.setSelected(ae.isOutputSide());
            meParameterName.setText(ae.getName());
            meValue.setText(Coerce.toString(ae.getValue()));
            meParamDesc.setText(ae.getDescription());
            meType.selectionModelProperty().get().select(ae.getType().getName());
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