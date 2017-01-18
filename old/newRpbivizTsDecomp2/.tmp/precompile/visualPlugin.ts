module powerbi.visuals.plugins {
    export var PBI_CV_Template = {
        name: 'PBI_CV_Template',
        displayName: 'Template',
        class: 'Visual',
        version: '1.0.0',
        apiVersion: '1.3.0',
        create: (options: extensibility.visual.VisualConstructorOptions) => new powerbi.extensibility.visual.PBI_CV_Template.Visual(options),
        custom: true
    };
}