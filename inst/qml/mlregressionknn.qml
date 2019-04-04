//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {

    VariablesForm {
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList {
            name: "target"
            title: qsTr("Target")
            singleVariable: true
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
                    name: "predictors"
                    title: qsTr("Predictors")
                    singleVariable: false
                    allowedColumns: ["scale", "ordinal"]
        }
    }
    
    GroupBox {
        title: qsTr("Tables")
        
        CheckBox { text: qsTr("Training error") ; name: "trainingAccuracy"}
        
        //RowLayout {
        //    Label {  text: qsTr("From"); Layout.leftMargin: 20 }
        //    TextField {
        //        name: "predictionsFrom"
        //        inputType: "integer"
        //        validator: IntValidator {bottom: 1; top: 9999}
        //        value: "1"
        //        fieldWidth: 60
        //    }
        //    Label {  text: qsTr("to") }
        //    TextField {
        //        name: "predictionsTo"
        //        inputType: "integer"
        //        validator: IntValidator {bottom: 1; top: 9999}
        //        value: "10"
        //        fieldWidth: 60
        //    }
        //}
        //CheckBox { text: qsTr("Predictions") ; name: "tablePredictions" ; checked: false}
        //CheckBox { text: qsTr("Distances") ; name: "tableDistances"}
        //CheckBox { text: qsTr("Weights") ; name: "tableWeights"}      
     }
    
    GroupBox {
        title: qsTr("Plots")
        
        CheckBox { text: qsTr("Mean squared error") ; name: "plotErrorVsK"; enabled: validationManual.checked ? false : true }
        CheckBox { text: qsTr("Predicted performance (training)") ; name: "predictedPerformanceTraining" }
        CheckBox { text: qsTr("Predicted performance (test)") ; name: "predictedPerformanceTest" }
    }
    
    Section {
        title: qsTr("Training parameters")

        GridLayout {
          RadioButtonGroup {
              title: qsTr("Model optimization")
              name: "modelOpt"
              
              RadioButton { text: qsTr("Leave-one-out")                 ; name: "validationLeaveOneOut"; checked: true; id: validationLeaveOneOut }
              RadioButton { name: "validationKFold"; childrenOnSameRow: true
                  IntegerField {
                      name: "noOfFolds"
                      afterLabel: qsTr("-fold cross-validation")
                      defaultValue: 3
                      min: 2
                      max: 15
                      fieldWidth: 25
                 }
              }
              RadioButton { text: qsTr("Manual")                        ; name: "validationManual"; id: validationManual }
          }
        
          GroupBox {
              IntegerField { name: "noOfNearestNeighbours"; text: qsTr("No. of nearest neighbors:") ; defaultValue: 1 ; min: 1; max: 999999; fieldWidth: 60; enabled: validationManual.checked }
              IntegerField { name: "maxK"; text: qsTr("Max. nearest neighbors:") ; defaultValue: 10 ; min: 1; max: 999999; fieldWidth: 60; enabled: validationManual.checked ? false : true }
              PercentField { name: "trainingDataManual"; text: qsTr("Data used for training:")       ; defaultValue: 80; enabled: validationManual.checked }
              ComboBox {
                  name: "distanceParameterManual"
                  label: qsTr("Distance:")
                  enabled: validationManual.checked
                  model: ListModel {
                      ListElement { key: 1                ; value: "1"; name: "Manhattan" }
                      ListElement { key: 2                ; value: "2"; name: "Euclidian" }
                  }
              }ComboBox {
                  name: "weights"
                  label: qsTr("Weights:")
                  enabled: validationManual.checked
                  model: ListModel {
                      ListElement { key: "Rectangular"                ; value: "rectangular" }
                      ListElement { key: "Epanechnikov"               ; value: "epanechnikov" }
                      ListElement { key: "Biweight"                   ; value: "biweight" }
                      ListElement { key: "Triweight"                  ; value: "triweight" }
                      ListElement { key: "Cosine"                     ; value: "cos" }
                      ListElement { key: "Inverse"                    ; value: "inv" }
                      ListElement { key: "Gaussian"                   ; value: "gaussian" }
                      ListElement { key: "Rank"                       ; value: "rank" }
                      ListElement { key: "Optimal"                    ; value: "optimal" }
                  }
              }
              CheckBox { text: qsTr("Scale variables") ; name: "scaleEqualSD"; checked: true}
              CheckBox { name: "seedBox"; text: qsTr("Set seed:"); childrenOnSameRow: true
                  DoubleField  { name: "seed"; defaultValue: 1; min: -999999; max: 999999; fieldWidth: 60 }}
          }
        }
    }
    
    Section {
      text: qsTr("Predictions")
      
          RadioButtonGroup
          {
              name: "applyModel"
              RadioButton { value: "noApp"         ; text: qsTr("Do not predict data"); checked: true        }
              RadioButton { value: "applyImpute"   ; text: qsTr("Predict missing values in target")  }
              RadioButton { value: "applyIndicator"; text: qsTr("Predict data according to apply indicator"); id: applyIndicator       }
          }
      
          VariablesForm {
          visible: applyIndicator.checked
              height: 150
              AvailableVariablesList { name: "predictionVariables"; allowedColumns: ["nominal"] }
              AssignedVariablesList {
                          name: "indicator"
                          title: qsTr("Apply indicator")
                          singleVariable: true
                          allowedColumns: ["nominal"]
              }
          }  
    }
}
