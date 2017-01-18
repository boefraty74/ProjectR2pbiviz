var powerbi;!function(e){var t;!function(e){var t;!function(e){var t;!function(e){function t(e,t,a,i){if(e){var s=e[t];if(s){var r=s[a];if(void 0!==r)return r}}return i}function a(e,t,a,i,s,r){if(e){var o=e[t];if(o){var n=o[a];if(n<s)return s;if(n>r)return r;if(void 0!==n)return n}}return i}function i(e,t,a,i,s,r){if(e){var o=e[t];if(o){var n=o[a];if(void 0!==n)return n>r?r:n<s?s:n}}return i}function s(e,t,a,i,s,r){return r&&t==a&&1==s?i:r&&t!=a&&0==s?i:e}function r(e,t){return"auto"!=t?"None":"auto"==t&&"None"==e?"fast":e}function o(e,t,a){return e<t?t:e>a?a:e}function n(e,t,a,i,s){var r=e.objects;if(r){var o=r[t];if(o){var n=o[a];if(n){var l=n[i];if(void 0!==l)return l}}}return s}function l(e,t,a,i){if(e){var s=e[t];if(s){var r=s[a];if(void 0!==r&&void 0!==r.solid&&void 0!==r.solid.color)return r.solid.color}}return i}e.getValue=t,e.getValueMinMax=a,e.getValueNumberMinMax=i,e.ifStringReturnString=s,e.ifStringReturnStringClustersMethod=r,e.inMinMax=o,e.getCategoricalObjectValue=n,e.getFillValue=l}(t=e.PBI_CV_EXAMPLE2||(e.PBI_CV_EXAMPLE2={}))}(t=e.visual||(e.visual={}))}(t=e.extensibility||(e.extensibility={}))}(powerbi||(powerbi={}));var powerbi;!function(e){var t;!function(e){var t;!function(e){var t;!function(e){var t=function(){function t(e){this.imageDiv=document.createElement("div"),this.imageDiv.className="rcv_autoScaleImageContainer",e.element.appendChild(this.imageDiv),this.imageElement=document.createElement("img"),this.imageElement.className="rcv_autoScaleImage",this.imageDiv.appendChild(this.imageElement),this.settings_model_params={modelType:"automatic",targetSeasonality:"autodetect from date",freq:12},this.settings_algo_params={degree:!1,robustToOutliers:!0,percentile:50},this.settings_plot_params={plotType:"all",weight:10,lineCol:"red",labelsCol:"orange",textSize:10},this.settings_extra_params={show:!0,textSize:10,infoCol:"brown"}}return t.prototype.update=function(t){var a=t.dataViews;if(a&&0!==a.length){var i=a[0];if(i&&i.metadata){this.settings_model_params={modelType:e.getValue(i.metadata.objects,"settings_model_params","modelType","automatic"),targetSeasonality:e.getValue(i.metadata.objects,"settings_model_params","targetSeasonality","autodetect from date"),freq:e.getValue(i.metadata.objects,"settings_model_params","freq",12)},this.settings_algo_params={degree:e.getValue(i.metadata.objects,"settings_algo_params","degree",!1),robustToOutliers:e.getValue(i.metadata.objects,"settings_algo_params","robustToOutliers",!0),percentile:e.getValue(i.metadata.objects,"settings_algo_params","percentile",50)},this.settings_plot_params={plotType:e.getValue(i.metadata.objects,"settings_plot_params","plotType","all"),weight:e.getValue(i.metadata.objects,"settings_plot_params","weight",10),lineCol:e.getValue(i.metadata.objects,"settings_plot_params","lineCol","red"),labelsCol:e.getValue(i.metadata.objects,"settings_plot_params","labelsCol","orange"),textSize:e.getValue(i.metadata.objects,"settings_plot_params","textSize",10)},this.settings_extra_params={show:e.getValue(i.metadata.objects,"settings_extra_params","show",!0),textSize:e.getValue(i.metadata.objects,"settings_extra_params","textSize",10),infoCol:e.getValue(i.metadata.objects,"settings_extra_params","infoCol","brown")};var s=null;i.scriptResult&&i.scriptResult.payloadBase64&&(s="data:image/png;base64,"+i.scriptResult.payloadBase64),s?this.imageElement.src=s:this.imageElement.src=null,this.onResizing(t.viewport)}}},t.prototype.onResizing=function(e){this.imageDiv.style.height=e.height+"px",this.imageDiv.style.width=e.width+"px"},t.prototype.enumerateObjectInstances=function(t){var a=t.objectName,i=[];switch(a){case"settings_model_params":i.push({objectName:a,properties:{modelType:this.settings_model_params.modelType,targetSeasonality:this.settings_model_params.targetSeasonality},selector:null}),"manual"==this.settings_model_params.targetSeasonality&&i.push({objectName:a,properties:{freq:e.inMinMax(this.settings_model_params.freq,1,1e4)},selector:null});break;case"settings_algo_params":i.push({objectName:a,properties:{degree:this.settings_algo_params.degree,robustToOutliers:this.settings_algo_params.robustToOutliers,percentile:e.inMinMax(this.settings_algo_params.percentile,1,100)},selector:null});break;case"settings_plot_params":i.push({objectName:a,properties:{plotType:this.settings_plot_params.plotType,weight:e.inMinMax(this.settings_plot_params.weight,1,50),lineCol:this.settings_plot_params.lineCol,labelsCol:this.settings_plot_params.labelsCol,textSize:e.inMinMax(this.settings_plot_params.textSize,8,40)},selector:null});break;case"settings_extra_params":i.push({objectName:a,properties:{show:this.settings_extra_params.show,textSize:e.inMinMax(this.settings_extra_params.textSize,8,40),infoCol:this.settings_extra_params.infoCol},selector:null})}return i},t}();e.Visual=t}(t=e.PBI_CV_EXAMPLE2||(e.PBI_CV_EXAMPLE2={}))}(t=e.visual||(e.visual={}))}(t=e.extensibility||(e.extensibility={}))}(powerbi||(powerbi={}));var powerbi;!function(e){var t;!function(t){var a;!function(t){t.PBI_CV_EXAMPLE2={name:"PBI_CV_EXAMPLE2",displayName:"Time series decomposition",class:"Visual",version:"1.0.0",apiVersion:"1.3.0",create:function(t){return new e.extensibility.visual.PBI_CV_EXAMPLE2.Visual(t)},custom:!0}}(a=t.plugins||(t.plugins={}))}(t=e.visuals||(e.visuals={}))}(powerbi||(powerbi={}));