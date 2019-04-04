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
        AssignedVariablesList  { name: "target"    ; title: qsTr("Target")         ; singleVariable: true; allowedColumns: ["nominal", "ordinal"]                  }
        AssignedVariablesList  { name: "predictors"; title: qsTr("Predictors")                                                                                     }
        AssignedVariablesList  { name: "indicator" ; title: qsTr("Apply indicator"); singleVariable: true; allowedColumns: ["nominal"]; enabled: indicator.checked }
    }

    GridLayout {

        GroupBox {
            title: qsTr("Tables")

            CheckBox { name: "classRanForConfTable"   ;	text: qsTr("Confusion table")    ; checked: true }
            CheckBox { name: "tableVariableImportance";	text: qsTr("Variable importance")                }
        }

        GroupBox {
            title: qsTr("Plots")

            CheckBox { name: "plotVarImp1";           text: qsTr("Mean decrease in accuracy")     }
            CheckBox { name: "plotVarImp2";           text: qsTr("Total increase in node purity") }
            CheckBox { name: "plotTreesVsModelError"; text: qsTr("Trees vs. model error")         }
        }

        GroupBox {
            title: qsTr("Model application")

            RadioButtonGroup {
                name: "applyModel"
                RadioButton { value: "noApp"         ; text: qsTr("Do not apply model")                     ; checked: true }
                RadioButton { value: "applyIndicator"; text: qsTr("Apply model according to indicator")     ; id: indicator }
                RadioButton { value: "applyImpute"   ; text: qsTr("Apply model to missing values in target")                }
            }
        }

    }

    ExpanderButton
    {
        title: qsTr("Model Specifications")

        GridLayout {

            GroupBox {
                IntegerField { name: "noOfTrees"     ; text: qsTr("No. of trees for training:")  ; defaultValue: 500 ; min: 1; max: 999999; fieldWidth: 60 }
                PercentField { name: "dataTrain"     ; text: qsTr("Data used for training:")     ; defaultValue: 80                                        }
                PercentField { name: "bagFrac"       ; text: qsTr("Training data used per tree:"); defaultValue: 50                                        }
            }

            RadioButtonGroup {
                title: qsTr("Predictors considered per split")
                name: "noOfPredictors"
                RadioButton { name: "auto"  ; text: qsTr("Auto")   ; checked: true}
                RadioButton { name: "manual"; text: qsTr("Manual") ; childrenOnSameRow: true
                    IntegerField { name: "numberOfPredictors"; min: 1; max: 999999; defaultValue: 1; fieldWidth: 60 }
                }
            }

        }

    }

    ExpanderButton
    {
        title: qsTr("Advanced")

        GroupBox {
            CheckBox { name: "seedBox"; text: qsTr("Set seed:"); childrenOnSameRow: true
                DoubleField  { name: "seed"; defaultValue: 1; min: -999999; max: 999999; fieldWidth: 60 }
            }
        }
    }

}
