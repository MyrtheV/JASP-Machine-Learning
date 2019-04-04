//
// Copyright (C) 2013-2019 University of Amsterdam
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
import JASP.Theme 1.0

// All Analysis forms must be built with the From QML item
Form
{
    usesJaspResults: true
    VariablesForm
    {
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList { name: "target";	title: qsTr("Target"); singleVariable: true; allowedColumns: ["scale"] }
        AssignedVariablesList { name: "predictors";	title: qsTr("Predictors") }
        AssignedVariablesList { name: "indicator";	title: qsTr("Apply indicator (optional)"); singleVariable: true; allowedColumns: ["nominal"]; enabled: indicator.checked }
    }

    GroupBox
    {

    title: qsTr("Model application")

    RadioButtonGroup
    {
        name: "applyModel"
        RadioButton { value: "noApp"         ; text: qsTr("Do not apply model")                     ; checked: true }
        RadioButton { value: "applyIndicator"; text: qsTr("Apply model according to indicator")     ; id: indicator }
        RadioButton { value: "applyImpute"   ; text: qsTr("Apply model to missing values in target")                }
    }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { name: "regBoostRelInfTable";	text: qsTr("Relative Influence Table")            }
    }

    ExpanderButton
    {
        title: qsTr("Model Specifications")

        GridLayout
        {

            RadioButtonGroup {
                title: qsTr("Number of trees for training")
                name: "noOfTrees"
                RadioButton { name: "auto"  ; text: qsTr("Auto")   ; checked: true}
                RadioButton { name: "manual"; text: qsTr("Manual") ; childrenOnSameRow: true
                    IntegerField { name: "numberOfTrees"; min: 1; max: 999999; defaultValue: 100; fieldWidth: 60 }
                }
            }

            RadioButtonGroup {
                title: qsTr("Shrinkage")
                name: "shrinkage"
                RadioButton { text: qsTr("Auto")    ; name: "auto"  ; checked: true}
                RadioButton { text: qsTr("Manual")  ; name: "manual"; childrenOnSameRow: true
                    DoubleField { name: "shrinkPar" ; defaultValue: 0.1; min: 0; max: 1; fieldWidth: 60 }
                }
            }

            RadioButtonGroup {
                title: qsTr("Interaction depth")
                name: "int.depth"
                RadioButton { text: qsTr("Auto")    ; name: "auto"  ; checked: true}
                RadioButton { text: qsTr("Manual")  ; name: "manual"; childrenOnSameRow: true
                    IntegerField { name: "int.depth.parameter"; defaultValue: 1; min: 1; max: 99; fieldWidth: 25 }
                }
            }

            RadioButtonGroup {
                title: qsTr("Min. no. observations in node")
                name: "nNode"
                RadioButton { text: qsTr("Auto")        ; name: "auto"  ; checked: true}
                RadioButton { text: qsTr("Manual")  ; name: "manual"; childrenOnSameRow: true
                    IntegerField { name: "nNodeSpec"; defaultValue: 10; min: 1; max: 999999 }
                }
            }

            RadioButtonGroup
            {
                title: qsTr("Data used for training")
                name: "dataTrain"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); childrenOnSameRow: true
                    PercentField { name: "percentageDataTraining"; defaultValue: 80 }
                }
            }

            RadioButtonGroup
            {
                title: qsTr("Training data used per tree")
                name: "bag.fraction"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); childrenOnSameRow: true
                    PercentField { name: "bag.fraction.spec"; defaultValue: 50 }
                }
            }

            RadioButtonGroup {
                title: qsTr("Model optimization")
                name: "modelOptimization"
                RadioButton { name: "cv"; childrenOnSameRow: true
                    IntegerField {
                        name: "cvFolds"
                        afterLabel: qsTr("-fold cross-validation")
                        defaultValue: 10
                        min: 2
                        max: 30
                        fieldWidth: 25
                   }
                }
                RadioButton { text: qsTr("Out-of-bag")              ; name: "oob"                  }
                RadioButton { text: qsTr("None")                    ; name: "noOpt"; checked: true }
            }

        }
    }

    ExpanderButton
    {
        title: qsTr("Plots")

        Group
        {
            CheckBox { name: "plotRelInf";          text: qsTr("Relative influence")       }
            CheckBox { name: "plotDeviance";        text: qsTr("Deviance")                 }
            CheckBox { name: "plotOOBChangeDev";    text: qsTr("OOB improvement")          }
            CheckBox { name: "plotPredPerformance"; text: qsTr("Predictive performance")   }
        }
    }

    ExpanderButton
    {
        title: qsTr("Advanced")

        GridLayout{

            GroupBox
            {

            title: qsTr("Set seed")

            RadioButtonGroup
            {
                name: "seedBox"
                RadioButton { value: "auto"  ;	text: qsTr("Auto")  ; checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); childrenOnSameRow: true
                    DoubleField { name: "seed"; defaultValue: 1 }
                }
            }
            }

            GroupBox
            {

            title: qsTr("Missing values")

            RadioButtonGroup
            {
                name: "NAs"
                RadioButton { value: "na.omit" ; text: qsTr("Omit all rows that contain NAs"); checked: true  }
                RadioButton { value: "roughfix"; text: qsTr("Apply roughfix to NAs")                          }
            }
            }

            GroupBox
            {

            title: qsTr("Distribution")

            RadioButtonGroup
            {
                name: "dist"
                RadioButton { value: "gaussian" ; text: qsTr("Gaussian"); checked: true  }
                RadioButton { value: "laplace"  ; text: qsTr("Laplace")                  }
                RadioButton { value: "tdist"    ; text: qsTr("t")                        }
            }
            }

        }
    }
}
