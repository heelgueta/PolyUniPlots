(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.module = f()}})(function(){var define,module,exports;return (function(){function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s}return e})()({1:[function(require,module,exports){

'use strict';

const options = [
    {"name":"data","type":"Data"},
    {"name":"vars","title":"Variables","type":"Variables","suggested":["ordinal","nominal"],"permitted":["factor"]},
    {"name":"chartType","title":"Chart type","type":"List","options":[{"name":"bars","title":"Stacked bars"},{"name":"waffle","title":"Waffle chart"},{"name":"pictogram","title":"Pictogram"},{"name":"parliament","title":"Parliament/arc"}],"default":"bars"},
    {"name":"diverging","title":"Diverging layout (Likert-style)","type":"Bool","default":false},
    {"name":"sortVars","title":"Sort variables","type":"List","options":[{"name":"none","title":"As entered"},{"name":"name","title":"Alphabetically"},{"name":"freq_first","title":"By first category"},{"name":"freq_last","title":"By last category"}],"default":"none"},
    {"name":"reverseVars","title":"Reverse variable order","type":"Bool","default":false},
    {"name":"showPct","title":"Show percentage labels","type":"Bool","default":true},
    {"name":"minPctLabel","title":"Min % to label","type":"Integer","default":5},
    {"name":"showN","title":"Show N per variable","type":"Bool","default":false},
    {"name":"colorScheme","title":"Color palette","type":"List","options":[{"name":"rdbulite","title":"Red–Blue (diverging)"},{"name":"rdylgn","title":"Red–Yellow–Green"},{"name":"piyg","title":"Pink–Green"},{"name":"prgn","title":"Purple–Green"},{"name":"pastel","title":"Pastel 1"},{"name":"dark2","title":"Dark 2"},{"name":"set2","title":"Set 2"},{"name":"viridis","title":"Viridis"},{"name":"plasma","title":"Plasma"}],"default":"set2"},
    {"name":"legendPos","title":"Legend position","type":"List","options":[{"name":"bottom","title":"Bottom"},{"name":"right","title":"Right"},{"name":"top","title":"Top"},{"name":"none","title":"None"}],"default":"bottom"},
    {"name":"legendTitle","title":"Legend title","type":"String","default":""},
    {"name":"themeChoice","title":"Theme","type":"List","options":[{"name":"minimal","title":"Minimal"},{"name":"classic","title":"Classic"},{"name":"bw","title":"Black & White"},{"name":"light","title":"Light"}],"default":"minimal"},
    {"name":"barHeight","title":"Bar height (%)","type":"Integer","default":70},
    {"name":"title","title":"Plot title","type":"String","default":""},
    {"name":"plotWidth","title":"Width (px)","type":"Integer","default":700},
    {"name":"plotHeight","title":"Height (px)","type":"Integer","default":400}
];

const view = function() {
    View.extend({ jus: "2.0", events: [] }).call(this);
}

view.layout = ui.extend({
    label: "Multiple Ordinal/Nominal Plots",
    jus: "2.0",
    type: "root",
    stage: 0,
    controls: [
        {
            type: DefaultControls.VariableSupplier,
            typeName: 'VariableSupplier',
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    type: DefaultControls.TargetLayoutBox,
                    typeName: 'TargetLayoutBox',
                    label: "Variables",
                    controls: [
                        {
                            type: DefaultControls.VariablesListBox,
                            typeName: 'VariablesListBox',
                            name: "vars",
                            isTarget: true
                        }
                    ]
                }
            ]
        },
        {
            type: DefaultControls.CollapseBox,
            typeName: 'CollapseBox',
            label: "Chart Type",
            collapsed: false,
            controls: [
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "chartType_bars",       optionName: "chartType", optionPart: "bars" },
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "chartType_waffle",     optionName: "chartType", optionPart: "waffle" },
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "chartType_pictogram",  optionName: "chartType", optionPart: "pictogram" },
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "chartType_parliament", optionName: "chartType", optionPart: "parliament" }
            ]
        },
        {
            type: DefaultControls.CollapseBox,
            typeName: 'CollapseBox',
            label: "Layout",
            collapsed: false,
            controls: [
                { type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "diverging" },
                { type: DefaultControls.ComboBox, typeName: 'ComboBox', name: "sortVars" },
                { type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "reverseVars" }
            ]
        },
        {
            type: DefaultControls.CollapseBox,
            typeName: 'CollapseBox',
            label: "Labels",
            collapsed: false,
            controls: [
                { type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "showPct" },
                { type: DefaultControls.TextBox,  typeName: 'TextBox',  name: "minPctLabel", label: "Min % to label", format: FormatDef.number },
                { type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "showN" }
            ]
        },
        {
            type: DefaultControls.CollapseBox,
            typeName: 'CollapseBox',
            label: "Appearance",
            collapsed: false,
            controls: [
                { type: DefaultControls.ComboBox, typeName: 'ComboBox', name: "colorScheme" },
                { type: DefaultControls.ComboBox, typeName: 'ComboBox', name: "legendPos" },
                { type: DefaultControls.TextBox,  typeName: 'TextBox',  name: "legendTitle", label: "Legend title", format: FormatDef.string },
                { type: DefaultControls.ComboBox, typeName: 'ComboBox', name: "themeChoice" },
                { type: DefaultControls.TextBox,  typeName: 'TextBox',  name: "barHeight", label: "Bar height (%)", format: FormatDef.number }
            ]
        },
        {
            type: DefaultControls.CollapseBox,
            typeName: 'CollapseBox',
            label: "Labels & Size",
            collapsed: true,
            controls: [
                { type: DefaultControls.TextBox, typeName: 'TextBox', name: "title",      label: "Title",       format: FormatDef.string },
                { type: DefaultControls.TextBox, typeName: 'TextBox', name: "plotWidth",  label: "Width (px)",  format: FormatDef.number },
                { type: DefaultControls.TextBox, typeName: 'TextBox', name: "plotHeight", label: "Height (px)", format: FormatDef.number }
            ]
        }
    ]
});

module.exports = { view : view, options: options };

},{}]},{},[1])(1)
});
