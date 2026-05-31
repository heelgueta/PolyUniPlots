(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.module = f()}})(function(){var define,module,exports;return (function(){function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s}return e})()({1:[function(require,module,exports){

'use strict';

const options = [
    {"name":"data","type":"Data"},
    {"name":"vars","title":"Variables","type":"Variables","suggested":["continuous"],"permitted":["numeric"]},
    {"name":"plotType","title":"Plot type","type":"List","options":[{"name":"box","title":"Box plot"},{"name":"violin","title":"Violin"},{"name":"strip","title":"Strip"},{"name":"histogram","title":"Histogram"},{"name":"ridge","title":"Ridge"}],"default":"box"},
    {"name":"orientation","title":"Orientation","type":"List","options":[{"name":"vertical","title":"Vertical"},{"name":"horizontal","title":"Horizontal"}],"default":"vertical"},
    {"name":"showJitter","title":"Overlay jitter","type":"Bool","default":false},
    {"name":"showMean","title":"Show mean","type":"Bool","default":false},
    {"name":"showMeanCI","title":"With confidence interval","type":"Bool","default":false},
    {"name":"ciWidth","title":"CI width (%)","type":"Integer","default":95},
    {"name":"showRug","title":"Rug","type":"Bool","default":false},
    {"name":"showOutliers","title":"Show outliers","type":"Bool","default":true},
    {"name":"violinScale","title":"Violin scale","type":"List","options":[{"name":"area","title":"Area"},{"name":"count","title":"Count"},{"name":"width","title":"Width"}],"default":"area"},
    {"name":"plotAlpha","title":"Transparency (%)","type":"Integer","default":80},
    {"name":"boxWidth","title":"Box/strip width (%)","type":"Integer","default":55},
    {"name":"jitterWidth","title":"Jitter spread (%)","type":"Integer","default":20},
    {"name":"colorScheme","title":"Color palette","type":"List","options":[{"name":"viridis","title":"Viridis"},{"name":"plasma","title":"Plasma"},{"name":"mako","title":"Mako"},{"name":"rocket","title":"Rocket"},{"name":"turbo","title":"Turbo"},{"name":"dark","title":"Dark 2"},{"name":"pastel","title":"Pastel 1"},{"name":"warm","title":"Warm"},{"name":"cold","title":"Cold"}],"default":"viridis"},
    {"name":"themeChoice","title":"Theme","type":"List","options":[{"name":"minimal","title":"Minimal"},{"name":"classic","title":"Classic"},{"name":"bw","title":"Black & White"},{"name":"light","title":"Light"}],"default":"minimal"},
    {"name":"title","title":"Plot title","type":"String","default":""},
    {"name":"plotWidth","title":"Width (px)","type":"Integer","default":650},
    {"name":"plotHeight","title":"Height (px)","type":"Integer","default":480}
];

const view = function() {
    View.extend({ jus: "2.0", events: [] }).call(this);
}

view.layout = ui.extend({
    label: "Multiple Numeric Plots",
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
            label: "Plot Type",
            collapsed: false,
            controls: [
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "plotType_box",       optionName: "plotType", optionPart: "box" },
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "plotType_violin",    optionName: "plotType", optionPart: "violin" },
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "plotType_strip",     optionName: "plotType", optionPart: "strip" },
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "plotType_histogram", optionName: "plotType", optionPart: "histogram" },
                { type: DefaultControls.RadioButton, typeName: 'RadioButton', name: "plotType_ridge",     optionName: "plotType", optionPart: "ridge" },
                { type: DefaultControls.ComboBox, typeName: 'ComboBox', name: "orientation" }
            ]
        },
        {
            type: DefaultControls.CollapseBox,
            typeName: 'CollapseBox',
            label: "Overlays",
            collapsed: false,
            controls: [
                { type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "showJitter" },
                { type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "showRug" },
                { type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "showOutliers" },
                {
                    type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "showMean",
                    controls: [
                        {
                            type: DefaultControls.CheckBox, typeName: 'CheckBox', name: "showMeanCI",
                            controls: [
                                { type: DefaultControls.TextBox, typeName: 'TextBox', name: "ciWidth", label: "CI %", format: FormatDef.number }
                            ]
                        }
                    ]
                }
            ]
        },
        {
            type: DefaultControls.CollapseBox,
            typeName: 'CollapseBox',
            label: "Appearance",
            collapsed: false,
            controls: [
                { type: DefaultControls.ComboBox, typeName: 'ComboBox', name: "colorScheme" },
                { type: DefaultControls.ComboBox, typeName: 'ComboBox', name: "themeChoice" },
                { type: DefaultControls.ComboBox, typeName: 'ComboBox', name: "violinScale" },
                { type: DefaultControls.TextBox,  typeName: 'TextBox',  name: "plotAlpha",   label: "Transparency (%)", format: FormatDef.number },
                { type: DefaultControls.TextBox,  typeName: 'TextBox',  name: "boxWidth",    label: "Box/strip width (%)", format: FormatDef.number },
                { type: DefaultControls.TextBox,  typeName: 'TextBox',  name: "jitterWidth", label: "Jitter spread (%)",   format: FormatDef.number }
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
