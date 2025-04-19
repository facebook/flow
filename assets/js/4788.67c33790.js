"use strict";(self.webpackChunknew_website=self.webpackChunknew_website||[]).push([[4788],{9854:(e,n,t)=>{t.d(n,{T:()=>l});var r=t(39142),o=t(89610),i=t(27422),s=t(94092),a=t(66401),u=t(8058),d=t(69592),h=t(69115),c=t(38207),f=t(89463),g="\0";class l{constructor(e={}){this._isDirected=!Object.prototype.hasOwnProperty.call(e,"directed")||e.directed,this._isMultigraph=!!Object.prototype.hasOwnProperty.call(e,"multigraph")&&e.multigraph,this._isCompound=!!Object.prototype.hasOwnProperty.call(e,"compound")&&e.compound,this._label=void 0,this._defaultNodeLabelFn=r.A(void 0),this._defaultEdgeLabelFn=r.A(void 0),this._nodes={},this._isCompound&&(this._parent={},this._children={},this._children[g]={}),this._in={},this._preds={},this._out={},this._sucs={},this._edgeObjs={},this._edgeLabels={}}isDirected(){return this._isDirected}isMultigraph(){return this._isMultigraph}isCompound(){return this._isCompound}setGraph(e){return this._label=e,this}graph(){return this._label}setDefaultNodeLabel(e){return o.A(e)||(e=r.A(e)),this._defaultNodeLabelFn=e,this}nodeCount(){return this._nodeCount}nodes(){return i.A(this._nodes)}sources(){var e=this;return s.A(this.nodes(),(function(n){return a.A(e._in[n])}))}sinks(){var e=this;return s.A(this.nodes(),(function(n){return a.A(e._out[n])}))}setNodes(e,n){var t=arguments,r=this;return u.A(e,(function(e){t.length>1?r.setNode(e,n):r.setNode(e)})),this}setNode(e,n){return Object.prototype.hasOwnProperty.call(this._nodes,e)?(arguments.length>1&&(this._nodes[e]=n),this):(this._nodes[e]=arguments.length>1?n:this._defaultNodeLabelFn(e),this._isCompound&&(this._parent[e]=g,this._children[e]={},this._children[g][e]=!0),this._in[e]={},this._preds[e]={},this._out[e]={},this._sucs[e]={},++this._nodeCount,this)}node(e){return this._nodes[e]}hasNode(e){return Object.prototype.hasOwnProperty.call(this._nodes,e)}removeNode(e){if(Object.prototype.hasOwnProperty.call(this._nodes,e)){var n=e=>this.removeEdge(this._edgeObjs[e]);delete this._nodes[e],this._isCompound&&(this._removeFromParentsChildList(e),delete this._parent[e],u.A(this.children(e),(e=>{this.setParent(e)})),delete this._children[e]),u.A(i.A(this._in[e]),n),delete this._in[e],delete this._preds[e],u.A(i.A(this._out[e]),n),delete this._out[e],delete this._sucs[e],--this._nodeCount}return this}setParent(e,n){if(!this._isCompound)throw new Error("Cannot set parent in a non-compound graph");if(d.A(n))n=g;else{for(var t=n+="";!d.A(t);t=this.parent(t))if(t===e)throw new Error("Setting "+n+" as parent of "+e+" would create a cycle");this.setNode(n)}return this.setNode(e),this._removeFromParentsChildList(e),this._parent[e]=n,this._children[n][e]=!0,this}_removeFromParentsChildList(e){delete this._children[this._parent[e]][e]}parent(e){if(this._isCompound){var n=this._parent[e];if(n!==g)return n}}children(e){if(d.A(e)&&(e=g),this._isCompound){var n=this._children[e];if(n)return i.A(n)}else{if(e===g)return this.nodes();if(this.hasNode(e))return[]}}predecessors(e){var n=this._preds[e];if(n)return i.A(n)}successors(e){var n=this._sucs[e];if(n)return i.A(n)}neighbors(e){var n=this.predecessors(e);if(n)return h.A(n,this.successors(e))}isLeaf(e){return 0===(this.isDirected()?this.successors(e):this.neighbors(e)).length}filterNodes(e){var n=new this.constructor({directed:this._isDirected,multigraph:this._isMultigraph,compound:this._isCompound});n.setGraph(this.graph());var t=this;u.A(this._nodes,(function(t,r){e(r)&&n.setNode(r,t)})),u.A(this._edgeObjs,(function(e){n.hasNode(e.v)&&n.hasNode(e.w)&&n.setEdge(e,t.edge(e))}));var r={};function o(e){var i=t.parent(e);return void 0===i||n.hasNode(i)?(r[e]=i,i):i in r?r[i]:o(i)}return this._isCompound&&u.A(n.nodes(),(function(e){n.setParent(e,o(e))})),n}setDefaultEdgeLabel(e){return o.A(e)||(e=r.A(e)),this._defaultEdgeLabelFn=e,this}edgeCount(){return this._edgeCount}edges(){return c.A(this._edgeObjs)}setPath(e,n){var t=this,r=arguments;return f.A(e,(function(e,o){return r.length>1?t.setEdge(e,o,n):t.setEdge(e,o),o})),this}setEdge(){var e,n,t,r,o=!1,i=arguments[0];"object"==typeof i&&null!==i&&"v"in i?(e=i.v,n=i.w,t=i.name,2===arguments.length&&(r=arguments[1],o=!0)):(e=i,n=arguments[1],t=arguments[3],arguments.length>2&&(r=arguments[2],o=!0)),e=""+e,n=""+n,d.A(t)||(t=""+t);var s=A(this._isDirected,e,n,t);if(Object.prototype.hasOwnProperty.call(this._edgeLabels,s))return o&&(this._edgeLabels[s]=r),this;if(!d.A(t)&&!this._isMultigraph)throw new Error("Cannot set a named edge when isMultigraph = false");this.setNode(e),this.setNode(n),this._edgeLabels[s]=o?r:this._defaultEdgeLabelFn(e,n,t);var a=function(e,n,t,r){var o=""+n,i=""+t;if(!e&&o>i){var s=o;o=i,i=s}var a={v:o,w:i};r&&(a.name=r);return a}(this._isDirected,e,n,t);return e=a.v,n=a.w,Object.freeze(a),this._edgeObjs[s]=a,v(this._preds[n],e),v(this._sucs[e],n),this._in[n][s]=a,this._out[e][s]=a,this._edgeCount++,this}edge(e,n,t){var r=1===arguments.length?w(this._isDirected,arguments[0]):A(this._isDirected,e,n,t);return this._edgeLabels[r]}hasEdge(e,n,t){var r=1===arguments.length?w(this._isDirected,arguments[0]):A(this._isDirected,e,n,t);return Object.prototype.hasOwnProperty.call(this._edgeLabels,r)}removeEdge(e,n,t){var r=1===arguments.length?w(this._isDirected,arguments[0]):A(this._isDirected,e,n,t),o=this._edgeObjs[r];return o&&(e=o.v,n=o.w,delete this._edgeLabels[r],delete this._edgeObjs[r],p(this._preds[n],e),p(this._sucs[e],n),delete this._in[n][r],delete this._out[e][r],this._edgeCount--),this}inEdges(e,n){var t=this._in[e];if(t){var r=c.A(t);return n?s.A(r,(function(e){return e.v===n})):r}}outEdges(e,n){var t=this._out[e];if(t){var r=c.A(t);return n?s.A(r,(function(e){return e.w===n})):r}}nodeEdges(e,n){var t=this.inEdges(e,n);if(t)return t.concat(this.outEdges(e,n))}}function v(e,n){e[n]?e[n]++:e[n]=1}function p(e,n){--e[n]||delete e[n]}function A(e,n,t,r){var o=""+n,i=""+t;if(!e&&o>i){var s=o;o=i,i=s}return o+"\x01"+i+"\x01"+(d.A(r)?"\0":r)}function w(e,n){return A(e,n.v,n.w,n.name)}l.prototype._nodeCount=0,l.prototype._edgeCount=0},80313:(e,n,t)=>{t.d(n,{T:()=>r.T});var r=t(9854)},94788:(e,n,t)=>{t.d(n,{Zp:()=>Be});var r=t(8058),o=t(5664),i=t(39142),s=t(34098),a=t(74722),u=t(91395),d=t(80313);class h{constructor(){var e={};e._next=e._prev=e,this._sentinel=e}dequeue(){var e=this._sentinel,n=e._prev;if(n!==e)return c(n),n}enqueue(e){var n=this._sentinel;e._prev&&e._next&&c(e),e._next=n._next,n._next._prev=e,n._next=e,e._prev=n}toString(){for(var e=[],n=this._sentinel,t=n._prev;t!==n;)e.push(JSON.stringify(t,f)),t=t._prev;return"["+e.join(", ")+"]"}}function c(e){e._prev._next=e._next,e._next._prev=e._prev,delete e._next,delete e._prev}function f(e,n){if("_next"!==e&&"_prev"!==e)return n}var g=i.A(1);function l(e,n){if(e.nodeCount()<=1)return[];var t=function(e,n){var t=new d.T,o=0,i=0;r.A(e.nodes(),(function(e){t.setNode(e,{v:e,in:0,out:0})})),r.A(e.edges(),(function(e){var r=t.edge(e.v,e.w)||0,s=n(e),a=r+s;t.setEdge(e.v,e.w,a),i=Math.max(i,t.node(e.v).out+=s),o=Math.max(o,t.node(e.w).in+=s)}));var s=u.A(i+o+3).map((function(){return new h})),a=o+1;return r.A(t.nodes(),(function(e){p(s,a,t.node(e))})),{graph:t,buckets:s,zeroIdx:a}}(e,n||g),o=function(e,n,t){var r,o=[],i=n[n.length-1],s=n[0];for(;e.nodeCount();){for(;r=s.dequeue();)v(e,n,t,r);for(;r=i.dequeue();)v(e,n,t,r);if(e.nodeCount())for(var a=n.length-2;a>0;--a)if(r=n[a].dequeue()){o=o.concat(v(e,n,t,r,!0));break}}return o}(t.graph,t.buckets,t.zeroIdx);return s.A(a.A(o,(function(n){return e.outEdges(n.v,n.w)})))}function v(e,n,t,o,i){var s=i?[]:void 0;return r.A(e.inEdges(o.v),(function(r){var o=e.edge(r),a=e.node(r.v);i&&s.push({v:r.v,w:r.w}),a.out-=o,p(n,t,a)})),r.A(e.outEdges(o.v),(function(r){var o=e.edge(r),i=r.w,s=e.node(i);s.in-=o,p(n,t,s)})),e.removeNode(o.v),s}function p(e,n,t){t.out?t.in?e[t.out-t.in+n].enqueue(t):e[e.length-1].enqueue(t):e[0].enqueue(t)}function A(e){var n="greedy"===e.graph().acyclicer?l(e,function(e){return function(n){return e.edge(n).weight}}(e)):function(e){var n=[],t={},o={};function i(s){Object.prototype.hasOwnProperty.call(o,s)||(o[s]=!0,t[s]=!0,r.A(e.outEdges(s),(function(e){Object.prototype.hasOwnProperty.call(t,e.w)?n.push(e):i(e.w)})),delete t[s])}return r.A(e.nodes(),i),n}(e);r.A(n,(function(n){var t=e.edge(n);e.removeEdge(n),t.forwardName=n.name,t.reversed=!0,e.setEdge(n.w,n.v,t,o.A("rev"))}))}var w=t(79790),b=t(25596),m=t(23068),y=t(93363),_=t(26666),E=t(59116),k=t(69592),x=t(86452),N=t(48585),O=t(81052);function P(e,n,t,r){var i;do{i=o.A(r)}while(e.hasNode(i));return t.dummy=n,e.setNode(i,t),i}function C(e){var n=new d.T({multigraph:e.isMultigraph()}).setGraph(e.graph());return r.A(e.nodes(),(function(t){e.children(t).length||n.setNode(t,e.node(t))})),r.A(e.edges(),(function(t){n.setEdge(t,e.edge(t))})),n}function L(e,n){var t,r,o=e.x,i=e.y,s=n.x-o,a=n.y-i,u=e.width/2,d=e.height/2;if(!s&&!a)throw new Error("Not possible to find intersection inside of the rectangle");return Math.abs(a)*u>Math.abs(s)*d?(a<0&&(d=-d),t=d*s/a,r=d):(s<0&&(u=-u),t=u,r=u*a/s),{x:o+t,y:i+r}}function j(e){var n=a.A(u.A(T(e)+1),(function(){return[]}));return r.A(e.nodes(),(function(t){var r=e.node(t),o=r.rank;k.A(o)||(n[o][r.order]=t)})),n}function I(e,n,t,r){var o={width:0,height:0};return arguments.length>=4&&(o.rank=t,o.order=r),P(e,"border",o,n)}function T(e){return y.A(a.A(e.nodes(),(function(n){var t=e.node(n).rank;if(!k.A(t))return t})))}function M(e,n){var t=O.A();try{return n()}finally{console.log(e+" time: "+(O.A()-t)+"ms")}}function R(e,n){return n()}function F(e,n,t,r,o,i){var s={width:0,height:0,rank:i,borderType:n},a=o[n][i-1],u=P(e,"border",s,t);o[n][i]=u,e.setParent(u,r),a&&e.setEdge(a,u,{weight:1})}function D(e){var n=e.graph().rankdir.toLowerCase();"bt"!==n&&"rl"!==n||function(e){r.A(e.nodes(),(function(n){V(e.node(n))})),r.A(e.edges(),(function(n){var t=e.edge(n);r.A(t.points,V),Object.prototype.hasOwnProperty.call(t,"y")&&V(t)}))}(e),"lr"!==n&&"rl"!==n||(!function(e){r.A(e.nodes(),(function(n){B(e.node(n))})),r.A(e.edges(),(function(n){var t=e.edge(n);r.A(t.points,B),Object.prototype.hasOwnProperty.call(t,"x")&&B(t)}))}(e),S(e))}function S(e){r.A(e.nodes(),(function(n){G(e.node(n))})),r.A(e.edges(),(function(n){G(e.edge(n))}))}function G(e){var n=e.width;e.width=e.height,e.height=n}function V(e){e.y=-e.y}function B(e){var n=e.x;e.x=e.y,e.y=n}function q(e){e.graph().dummyChains=[],r.A(e.edges(),(function(n){!function(e,n){var t=n.v,r=e.node(t).rank,o=n.w,i=e.node(o).rank,s=n.name,a=e.edge(n),u=a.labelRank;if(i===r+1)return;e.removeEdge(n);var d,h,c=void 0;for(h=0,++r;r<i;++h,++r)a.points=[],d=P(e,"edge",c={width:0,height:0,edgeLabel:a,edgeObj:n,rank:r},"_d"),r===u&&(c.width=a.width,c.height=a.height,c.dummy="edge-label",c.labelpos=a.labelpos),e.setEdge(t,d,{weight:a.weight},s),0===h&&e.graph().dummyChains.push(d),t=d;e.setEdge(t,o,{weight:a.weight},s)}(e,n)}))}var Y=t(88389);function z(e){var n={};r.A(e.sources(),(function t(r){var o=e.node(r);if(Object.prototype.hasOwnProperty.call(n,r))return o.rank;n[r]=!0;var i=x.A(a.A(e.outEdges(r),(function(n){return t(n.w)-e.edge(n).minlen})));return i!==Number.POSITIVE_INFINITY&&null!=i||(i=0),o.rank=i}))}function J(e,n){return e.node(n.w).rank-e.node(n.v).rank-e.edge(n).minlen}function Z(e){var n,t,r=new d.T({directed:!1}),o=e.nodes()[0],i=e.nodeCount();for(r.setNode(o,{});H(r,e)<i;)n=K(r,e),t=r.hasNode(n.v)?J(e,n):-J(e,n),Q(r,e,t);return r}function H(e,n){return r.A(e.nodes(),(function t(o){r.A(n.nodeEdges(o),(function(r){var i=r.v,s=o===i?r.w:i;e.hasNode(s)||J(n,r)||(e.setNode(s,{}),e.setEdge(o,s,{}),t(s))}))})),e.nodeCount()}function K(e,n){return Y.A(n.edges(),(function(t){if(e.hasNode(t.v)!==e.hasNode(t.w))return J(n,t)}))}function Q(e,n,t){r.A(e.nodes(),(function(e){n.node(e).rank+=t}))}var U=t(16145),W=t(94092);i.A(1);i.A(1);t(38919);function X(){}X.prototype=new Error;var $=t(92049);function ee(e,n,t){$.A(n)||(n=[n]);var o=(e.isDirected()?e.successors:e.neighbors).bind(e),i=[],s={};return r.A(n,(function(n){if(!e.hasNode(n))throw new Error("Graph does not have node: "+n);ne(e,n,"post"===t,s,o,i)})),i}function ne(e,n,t,o,i,s){Object.prototype.hasOwnProperty.call(o,n)||(o[n]=!0,t||s.push(n),r.A(i(n),(function(n){ne(e,n,t,o,i,s)})),t&&s.push(n))}t(9854);function te(e){e=function(e){var n=(new d.T).setGraph(e.graph());return r.A(e.nodes(),(function(t){n.setNode(t,e.node(t))})),r.A(e.edges(),(function(t){var r=n.edge(t.v,t.w)||{weight:0,minlen:1},o=e.edge(t);n.setEdge(t.v,t.w,{weight:r.weight+o.weight,minlen:Math.max(r.minlen,o.minlen)})})),n}(e),z(e);var n,t=Z(e);for(ie(t),re(t,e);n=ae(t);)de(t,e,n,ue(t,e,n))}function re(e,n){var t=function(e,n){return ee(e,n,"post")}(e,e.nodes());t=t.slice(0,t.length-1),r.A(t,(function(t){!function(e,n,t){var r=e.node(t),o=r.parent;e.edge(t,o).cutvalue=oe(e,n,t)}(e,n,t)}))}function oe(e,n,t){var o=e.node(t).parent,i=!0,s=n.edge(t,o),a=0;return s||(i=!1,s=n.edge(o,t)),a=s.weight,r.A(n.nodeEdges(t),(function(r){var s,u,d=r.v===t,h=d?r.w:r.v;if(h!==o){var c=d===i,f=n.edge(r).weight;if(a+=c?f:-f,s=t,u=h,e.hasEdge(s,u)){var g=e.edge(t,h).cutvalue;a+=c?-g:g}}})),a}function ie(e,n){arguments.length<2&&(n=e.nodes()[0]),se(e,{},1,n)}function se(e,n,t,o,i){var s=t,a=e.node(o);return n[o]=!0,r.A(e.neighbors(o),(function(r){Object.prototype.hasOwnProperty.call(n,r)||(t=se(e,n,t,r,o))})),a.low=s,a.lim=t++,i?a.parent=i:delete a.parent,t}function ae(e){return U.A(e.edges(),(function(n){return e.edge(n).cutvalue<0}))}function ue(e,n,t){var r=t.v,o=t.w;n.hasEdge(r,o)||(r=t.w,o=t.v);var i=e.node(r),s=e.node(o),a=i,u=!1;i.lim>s.lim&&(a=s,u=!0);var d=W.A(n.edges(),(function(n){return u===he(e,e.node(n.v),a)&&u!==he(e,e.node(n.w),a)}));return Y.A(d,(function(e){return J(n,e)}))}function de(e,n,t,o){var i=t.v,s=t.w;e.removeEdge(i,s),e.setEdge(o.v,o.w,{}),ie(e),re(e,n),function(e,n){var t=U.A(e.nodes(),(function(e){return!n.node(e).parent})),o=function(e,n){return ee(e,n,"pre")}(e,t);o=o.slice(1),r.A(o,(function(t){var r=e.node(t).parent,o=n.edge(t,r),i=!1;o||(o=n.edge(r,t),i=!0),n.node(t).rank=n.node(r).rank+(i?o.minlen:-o.minlen)}))}(e,n)}function he(e,n,t){return t.low<=n.lim&&n.lim<=t.lim}function ce(e){switch(e.graph().ranker){case"network-simplex":default:ge(e);break;case"tight-tree":!function(e){z(e),Z(e)}(e);break;case"longest-path":fe(e)}}te.initLowLimValues=ie,te.initCutValues=re,te.calcCutValue=oe,te.leaveEdge=ae,te.enterEdge=ue,te.exchangeEdges=de;var fe=z;function ge(e){te(e)}var le=t(38207),ve=t(89463);function pe(e){var n=P(e,"root",{},"_root"),t=function(e){var n={};function t(o,i){var s=e.children(o);s&&s.length&&r.A(s,(function(e){t(e,i+1)})),n[o]=i}return r.A(e.children(),(function(e){t(e,1)})),n}(e),o=y.A(le.A(t))-1,i=2*o+1;e.graph().nestingRoot=n,r.A(e.edges(),(function(n){e.edge(n).minlen*=i}));var s=function(e){return ve.A(e.edges(),(function(n,t){return n+e.edge(t).weight}),0)}(e)+1;r.A(e.children(),(function(r){Ae(e,n,i,s,o,t,r)})),e.graph().nodeRankFactor=i}function Ae(e,n,t,o,i,s,a){var u=e.children(a);if(u.length){var d=I(e,"_bt"),h=I(e,"_bb"),c=e.node(a);e.setParent(d,a),c.borderTop=d,e.setParent(h,a),c.borderBottom=h,r.A(u,(function(r){Ae(e,n,t,o,i,s,r);var u=e.node(r),c=u.borderTop?u.borderTop:r,f=u.borderBottom?u.borderBottom:r,g=u.borderTop?o:2*o,l=c!==f?1:i-s[a]+1;e.setEdge(d,c,{weight:g,minlen:l,nestingEdge:!0}),e.setEdge(f,h,{weight:g,minlen:l,nestingEdge:!0})})),e.parent(a)||e.setEdge(n,d,{weight:0,minlen:i+s[a]})}else a!==n&&e.setEdge(n,a,{weight:0,minlen:t})}var we=t(20903);function be(e,n,t){var i=function(e){var n;for(;e.hasNode(n=o.A("_root")););return n}(e),s=new d.T({compound:!0}).setGraph({root:i}).setDefaultNodeLabel((function(n){return e.node(n)}));return r.A(e.nodes(),(function(o){var a=e.node(o),u=e.parent(o);(a.rank===n||a.minRank<=n&&n<=a.maxRank)&&(s.setNode(o),s.setParent(o,u||i),r.A(e[t](o),(function(n){var t=n.v===o?n.w:n.v,r=s.edge(t,o),i=k.A(r)?0:r.weight;s.setEdge(t,o,{weight:e.edge(n).weight+i})})),Object.prototype.hasOwnProperty.call(a,"minRank")&&s.setNode(o,{borderLeft:a.borderLeft[n],borderRight:a.borderRight[n]}))})),s}var me=t(13354),ye=t(71244);function _e(e,n){for(var t=0,r=1;r<n.length;++r)t+=Ee(e,n[r-1],n[r]);return t}function Ee(e,n,t){for(var o=me.A(t,a.A(t,(function(e,n){return n}))),i=s.A(a.A(n,(function(n){return ye.A(a.A(e.outEdges(n),(function(n){return{pos:o[n.w],weight:e.edge(n).weight}})),"pos")}))),u=1;u<t.length;)u<<=1;var d=2*u-1;u-=1;var h=a.A(new Array(d),(function(){return 0})),c=0;return r.A(i.forEach((function(e){var n=e.pos+u;h[n]+=e.weight;for(var t=0;n>0;)n%2&&(t+=h[n+1]),h[n=n-1>>1]+=e.weight;c+=e.weight*t}))),c}function ke(e,n){var t={};return r.A(e,(function(e,n){var r=t[e.v]={indegree:0,in:[],out:[],vs:[e.v],i:n};k.A(e.barycenter)||(r.barycenter=e.barycenter,r.weight=e.weight)})),r.A(n.edges(),(function(e){var n=t[e.v],r=t[e.w];k.A(n)||k.A(r)||(r.indegree++,n.out.push(t[e.w]))})),function(e){var n=[];function t(e){return function(n){n.merged||(k.A(n.barycenter)||k.A(e.barycenter)||n.barycenter>=e.barycenter)&&function(e,n){var t=0,r=0;e.weight&&(t+=e.barycenter*e.weight,r+=e.weight);n.weight&&(t+=n.barycenter*n.weight,r+=n.weight);e.vs=n.vs.concat(e.vs),e.barycenter=t/r,e.weight=r,e.i=Math.min(n.i,e.i),n.merged=!0}(e,n)}}function o(n){return function(t){t.in.push(n),0==--t.indegree&&e.push(t)}}for(;e.length;){var i=e.pop();n.push(i),r.A(i.in.reverse(),t(i)),r.A(i.out,o(i))}return a.A(W.A(n,(function(e){return!e.merged})),(function(e){return b.A(e,["vs","i","barycenter","weight"])}))}(W.A(t,(function(e){return!e.indegree})))}function xe(e,n){var t,o=function(e,n){var t={lhs:[],rhs:[]};return r.A(e,(function(e){n(e)?t.lhs.push(e):t.rhs.push(e)})),t}(e,(function(e){return Object.prototype.hasOwnProperty.call(e,"barycenter")})),i=o.lhs,a=ye.A(o.rhs,(function(e){return-e.i})),u=[],d=0,h=0,c=0;i.sort((t=!!n,function(e,n){return e.barycenter<n.barycenter?-1:e.barycenter>n.barycenter?1:t?n.i-e.i:e.i-n.i})),c=Ne(u,a,c),r.A(i,(function(e){c+=e.vs.length,u.push(e.vs),d+=e.barycenter*e.weight,h+=e.weight,c=Ne(u,a,c)}));var f={vs:s.A(u)};return h&&(f.barycenter=d/h,f.weight=h),f}function Ne(e,n,t){for(var r;n.length&&(r=_.A(n)).i<=t;)n.pop(),e.push(r.vs),t++;return t}function Oe(e,n,t,o){var i=e.children(n),u=e.node(n),d=u?u.borderLeft:void 0,h=u?u.borderRight:void 0,c={};d&&(i=W.A(i,(function(e){return e!==d&&e!==h})));var f=function(e,n){return a.A(n,(function(n){var t=e.inEdges(n);if(t.length){var r=ve.A(t,(function(n,t){var r=e.edge(t),o=e.node(t.v);return{sum:n.sum+r.weight*o.order,weight:n.weight+r.weight}}),{sum:0,weight:0});return{v:n,barycenter:r.sum/r.weight,weight:r.weight}}return{v:n}}))}(e,i);r.A(f,(function(n){if(e.children(n.v).length){var r=Oe(e,n.v,t,o);c[n.v]=r,Object.prototype.hasOwnProperty.call(r,"barycenter")&&(i=n,s=r,k.A(i.barycenter)?(i.barycenter=s.barycenter,i.weight=s.weight):(i.barycenter=(i.barycenter*i.weight+s.barycenter*s.weight)/(i.weight+s.weight),i.weight+=s.weight))}var i,s}));var g=ke(f,t);!function(e,n){r.A(e,(function(e){e.vs=s.A(e.vs.map((function(e){return n[e]?n[e].vs:e})))}))}(g,c);var l=xe(g,o);if(d&&(l.vs=s.A([d,l.vs,h]),e.predecessors(d).length)){var v=e.node(e.predecessors(d)[0]),p=e.node(e.predecessors(h)[0]);Object.prototype.hasOwnProperty.call(l,"barycenter")||(l.barycenter=0,l.weight=0),l.barycenter=(l.barycenter*l.weight+v.order+p.order)/(l.weight+2),l.weight+=2}return l}function Pe(e){var n=T(e),t=Ce(e,u.A(1,n+1),"inEdges"),o=Ce(e,u.A(n-1,-1,-1),"outEdges"),i=function(e){var n={},t=W.A(e.nodes(),(function(n){return!e.children(n).length})),o=y.A(a.A(t,(function(n){return e.node(n).rank}))),i=a.A(u.A(o+1),(function(){return[]})),s=ye.A(t,(function(n){return e.node(n).rank}));return r.A(s,(function t(o){if(!N.A(n,o)){n[o]=!0;var s=e.node(o);i[s.rank].push(o),r.A(e.successors(o),t)}})),i}(e);je(e,i);for(var s,d=Number.POSITIVE_INFINITY,h=0,c=0;c<4;++h,++c){Le(h%2?t:o,h%4>=2);var f=_e(e,i=j(e));f<d&&(c=0,s=we.A(i),d=f)}je(e,s)}function Ce(e,n,t){return a.A(n,(function(n){return be(e,n,t)}))}function Le(e,n){var t=new d.T;r.A(e,(function(e){var o=e.graph().root,i=Oe(e,o,t,n);r.A(i.vs,(function(n,t){e.node(n).order=t})),function(e,n,t){var o,i={};r.A(t,(function(t){for(var r,s,a=e.parent(t);a;){if((r=e.parent(a))?(s=i[r],i[r]=a):(s=o,o=a),s&&s!==a)return void n.setEdge(s,a);a=r}}))}(e,t,i.vs)}))}function je(e,n){r.A(n,(function(n){r.A(n,(function(n,t){e.node(n).order=t}))}))}function Ie(e){var n=function(e){var n={},t=0;function o(i){var s=t;r.A(e.children(i),o),n[i]={low:s,lim:t++}}return r.A(e.children(),o),n}(e);r.A(e.graph().dummyChains,(function(t){for(var r=e.node(t),o=r.edgeObj,i=function(e,n,t,r){var o,i,s=[],a=[],u=Math.min(n[t].low,n[r].low),d=Math.max(n[t].lim,n[r].lim);o=t;do{o=e.parent(o),s.push(o)}while(o&&(n[o].low>u||d>n[o].lim));i=o,o=r;for(;(o=e.parent(o))!==i;)a.push(o);return{path:s.concat(a.reverse()),lca:i}}(e,n,o.v,o.w),s=i.path,a=i.lca,u=0,d=s[u],h=!0;t!==o.w;){if(r=e.node(t),h){for(;(d=s[u])!==a&&e.node(d).maxRank<r.rank;)u++;d===a&&(h=!1)}if(!h){for(;u<s.length-1&&e.node(d=s[u+1]).minRank<=r.rank;)u++;d=s[u]}e.setParent(t,d),t=e.successors(t)[0]}}))}var Te=t(12239),Me=t(82932);function Re(e,n){var t={};return ve.A(n,(function(n,o){var i=0,s=0,a=n.length,u=_.A(o);return r.A(o,(function(n,d){var h=function(e,n){if(e.node(n).dummy)return U.A(e.predecessors(n),(function(n){return e.node(n).dummy}))}(e,n),c=h?e.node(h).order:a;(h||n===u)&&(r.A(o.slice(s,d+1),(function(n){r.A(e.predecessors(n),(function(r){var o=e.node(r),s=o.order;!(s<i||c<s)||o.dummy&&e.node(n).dummy||Fe(t,r,n)}))})),s=d+1,i=c)})),o})),t}function Fe(e,n,t){if(n>t){var r=n;n=t,t=r}var o=e[n];o||(e[n]=o={}),o[t]=!0}function De(e,n,t){if(n>t){var r=n;n=t,t=r}return!!e[n]&&Object.prototype.hasOwnProperty.call(e[n],t)}function Se(e,n,t,o,i){var s={},a=function(e,n,t,o){var i=new d.T,s=e.graph(),a=function(e,n,t){return function(r,o,i){var s,a=r.node(o),u=r.node(i),d=0;if(d+=a.width/2,Object.prototype.hasOwnProperty.call(a,"labelpos"))switch(a.labelpos.toLowerCase()){case"l":s=-a.width/2;break;case"r":s=a.width/2}if(s&&(d+=t?s:-s),s=0,d+=(a.dummy?n:e)/2,d+=(u.dummy?n:e)/2,d+=u.width/2,Object.prototype.hasOwnProperty.call(u,"labelpos"))switch(u.labelpos.toLowerCase()){case"l":s=u.width/2;break;case"r":s=-u.width/2}return s&&(d+=t?s:-s),s=0,d}}(s.nodesep,s.edgesep,o);return r.A(n,(function(n){var o;r.A(n,(function(n){var r=t[n];if(i.setNode(r),o){var s=t[o],u=i.edge(s,r);i.setEdge(s,r,Math.max(a(e,n,o),u||0))}o=n}))})),i}(e,n,t,i),u=i?"borderLeft":"borderRight";function h(e,n){for(var t=a.nodes(),r=t.pop(),o={};r;)o[r]?e(r):(o[r]=!0,t.push(r),t=t.concat(n(r))),r=t.pop()}return h((function(e){s[e]=a.inEdges(e).reduce((function(e,n){return Math.max(e,s[n.v]+a.edge(n))}),0)}),a.predecessors.bind(a)),h((function(n){var t=a.outEdges(n).reduce((function(e,n){return Math.min(e,s[n.w]-a.edge(n))}),Number.POSITIVE_INFINITY),r=e.node(n);t!==Number.POSITIVE_INFINITY&&r.borderType!==u&&(s[n]=Math.max(s[n],t))}),a.successors.bind(a)),r.A(o,(function(e){s[e]=s[t[e]]})),s}function Ge(e){var n,t=j(e),o=w.A(Re(e,t),function(e,n){var t={};function o(n,o,i,s,a){var d;r.A(u.A(o,i),(function(o){d=n[o],e.node(d).dummy&&r.A(e.predecessors(d),(function(n){var r=e.node(n);r.dummy&&(r.order<s||r.order>a)&&Fe(t,n,d)}))}))}return ve.A(n,(function(n,t){var i,s=-1,a=0;return r.A(t,(function(r,u){if("border"===e.node(r).dummy){var d=e.predecessors(r);d.length&&(i=e.node(d[0]).order,o(t,a,u,s,i),a=u,s=i)}o(t,a,t.length,i,n.length)})),t})),t}(e,t)),i={};r.A(["u","d"],(function(s){n="u"===s?t:le.A(t).reverse(),r.A(["l","r"],(function(t){"r"===t&&(n=a.A(n,(function(e){return le.A(e).reverse()})));var u=("u"===s?e.predecessors:e.successors).bind(e),d=function(e,n,t,o){var i={},s={},a={};return r.A(n,(function(e){r.A(e,(function(e,n){i[e]=e,s[e]=e,a[e]=n}))})),r.A(n,(function(e){var n=-1;r.A(e,(function(e){var r=o(e);if(r.length){r=ye.A(r,(function(e){return a[e]}));for(var u=(r.length-1)/2,d=Math.floor(u),h=Math.ceil(u);d<=h;++d){var c=r[d];s[e]===e&&n<a[c]&&!De(t,e,c)&&(s[c]=e,s[e]=i[e]=i[c],n=a[c])}}}))})),{root:i,align:s}}(0,n,o,u),h=Se(e,n,d.root,d.align,"r"===t);"r"===t&&(h=E.A(h,(function(e){return-e}))),i[s+t]=h}))}));var s=function(e,n){return Y.A(le.A(n),(function(n){var t=Number.NEGATIVE_INFINITY,r=Number.POSITIVE_INFINITY;return Me.A(n,(function(n,o){var i=function(e,n){return e.node(n).width}(e,o)/2;t=Math.max(n+i,t),r=Math.min(n-i,r)})),t-r}))}(e,i);return function(e,n){var t=le.A(n),o=x.A(t),i=y.A(t);r.A(["u","d"],(function(t){r.A(["l","r"],(function(r){var s,a=t+r,u=e[a];if(u!==n){var d=le.A(u);(s="l"===r?o-x.A(d):i-y.A(d))&&(e[a]=E.A(u,(function(e){return e+s})))}}))}))}(i,s),function(e,n){return E.A(e.ul,(function(t,r){if(n)return e[n.toLowerCase()][r];var o=ye.A(a.A(e,r));return(o[1]+o[2])/2}))}(i,e.graph().align)}function Ve(e){(function(e){var n=j(e),t=e.graph().ranksep,o=0;r.A(n,(function(n){var i=y.A(a.A(n,(function(n){return e.node(n).height})));r.A(n,(function(n){e.node(n).y=o+i/2})),o+=i+t}))})(e=C(e)),Te.A(Ge(e),(function(n,t){e.node(t).x=n}))}function Be(e,n){var t=n&&n.debugTiming?M:R;t("layout",(()=>{var n=t("  buildLayoutGraph",(()=>function(e){var n=new d.T({multigraph:!0,compound:!0}),t=We(e.graph());return n.setGraph(w.A({},Ye,Ue(t,qe),b.A(t,ze))),r.A(e.nodes(),(function(t){var r=We(e.node(t));n.setNode(t,m.A(Ue(r,Je),Ze)),n.setParent(t,e.parent(t))})),r.A(e.edges(),(function(t){var r=We(e.edge(t));n.setEdge(t,w.A({},Ke,Ue(r,He),b.A(r,Qe)))})),n}(e)));t("  runLayout",(()=>function(e,n){n("    makeSpaceForEdgeLabels",(()=>function(e){var n=e.graph();n.ranksep/=2,r.A(e.edges(),(function(t){var r=e.edge(t);r.minlen*=2,"c"!==r.labelpos.toLowerCase()&&("TB"===n.rankdir||"BT"===n.rankdir?r.width+=r.labeloffset:r.height+=r.labeloffset)}))}(e))),n("    removeSelfEdges",(()=>function(e){r.A(e.edges(),(function(n){if(n.v===n.w){var t=e.node(n.v);t.selfEdges||(t.selfEdges=[]),t.selfEdges.push({e:n,label:e.edge(n)}),e.removeEdge(n)}}))}(e))),n("    acyclic",(()=>A(e))),n("    nestingGraph.run",(()=>pe(e))),n("    rank",(()=>ce(C(e)))),n("    injectEdgeLabelProxies",(()=>function(e){r.A(e.edges(),(function(n){var t=e.edge(n);if(t.width&&t.height){var r=e.node(n.v),o={rank:(e.node(n.w).rank-r.rank)/2+r.rank,e:n};P(e,"edge-proxy",o,"_ep")}}))}(e))),n("    removeEmptyRanks",(()=>function(e){var n=x.A(a.A(e.nodes(),(function(n){return e.node(n).rank}))),t=[];r.A(e.nodes(),(function(r){var o=e.node(r).rank-n;t[o]||(t[o]=[]),t[o].push(r)}));var o=0,i=e.graph().nodeRankFactor;r.A(t,(function(n,t){k.A(n)&&t%i!=0?--o:o&&r.A(n,(function(n){e.node(n).rank+=o}))}))}(e))),n("    nestingGraph.cleanup",(()=>function(e){var n=e.graph();e.removeNode(n.nestingRoot),delete n.nestingRoot,r.A(e.edges(),(function(n){e.edge(n).nestingEdge&&e.removeEdge(n)}))}(e))),n("    normalizeRanks",(()=>function(e){var n=x.A(a.A(e.nodes(),(function(n){return e.node(n).rank})));r.A(e.nodes(),(function(t){var r=e.node(t);N.A(r,"rank")&&(r.rank-=n)}))}(e))),n("    assignRankMinMax",(()=>function(e){var n=0;r.A(e.nodes(),(function(t){var r=e.node(t);r.borderTop&&(r.minRank=e.node(r.borderTop).rank,r.maxRank=e.node(r.borderBottom).rank,n=y.A(n,r.maxRank))})),e.graph().maxRank=n}(e))),n("    removeEdgeLabelProxies",(()=>function(e){r.A(e.nodes(),(function(n){var t=e.node(n);"edge-proxy"===t.dummy&&(e.edge(t.e).labelRank=t.rank,e.removeNode(n))}))}(e))),n("    normalize.run",(()=>q(e))),n("    parentDummyChains",(()=>Ie(e))),n("    addBorderSegments",(()=>function(e){r.A(e.children(),(function n(t){var o=e.children(t),i=e.node(t);if(o.length&&r.A(o,n),Object.prototype.hasOwnProperty.call(i,"minRank")){i.borderLeft=[],i.borderRight=[];for(var s=i.minRank,a=i.maxRank+1;s<a;++s)F(e,"borderLeft","_bl",t,i,s),F(e,"borderRight","_br",t,i,s)}}))}(e))),n("    order",(()=>Pe(e))),n("    insertSelfEdges",(()=>function(e){var n=j(e);r.A(n,(function(n){var t=0;r.A(n,(function(n,o){var i=e.node(n);i.order=o+t,r.A(i.selfEdges,(function(n){P(e,"selfedge",{width:n.label.width,height:n.label.height,rank:i.rank,order:o+ ++t,e:n.e,label:n.label},"_se")})),delete i.selfEdges}))}))}(e))),n("    adjustCoordinateSystem",(()=>function(e){var n=e.graph().rankdir.toLowerCase();"lr"!==n&&"rl"!==n||S(e)}(e))),n("    position",(()=>Ve(e))),n("    positionSelfEdges",(()=>function(e){r.A(e.nodes(),(function(n){var t=e.node(n);if("selfedge"===t.dummy){var r=e.node(t.e.v),o=r.x+r.width/2,i=r.y,s=t.x-o,a=r.height/2;e.setEdge(t.e,t.label),e.removeNode(n),t.label.points=[{x:o+2*s/3,y:i-a},{x:o+5*s/6,y:i-a},{x:o+s,y:i},{x:o+5*s/6,y:i+a},{x:o+2*s/3,y:i+a}],t.label.x=t.x,t.label.y=t.y}}))}(e))),n("    removeBorderNodes",(()=>function(e){r.A(e.nodes(),(function(n){if(e.children(n).length){var t=e.node(n),r=e.node(t.borderTop),o=e.node(t.borderBottom),i=e.node(_.A(t.borderLeft)),s=e.node(_.A(t.borderRight));t.width=Math.abs(s.x-i.x),t.height=Math.abs(o.y-r.y),t.x=i.x+t.width/2,t.y=r.y+t.height/2}})),r.A(e.nodes(),(function(n){"border"===e.node(n).dummy&&e.removeNode(n)}))}(e))),n("    normalize.undo",(()=>function(e){r.A(e.graph().dummyChains,(function(n){var t,r=e.node(n),o=r.edgeLabel;for(e.setEdge(r.edgeObj,o);r.dummy;)t=e.successors(n)[0],e.removeNode(n),o.points.push({x:r.x,y:r.y}),"edge-label"===r.dummy&&(o.x=r.x,o.y=r.y,o.width=r.width,o.height=r.height),n=t,r=e.node(n)}))}(e))),n("    fixupEdgeLabelCoords",(()=>function(e){r.A(e.edges(),(function(n){var t=e.edge(n);if(Object.prototype.hasOwnProperty.call(t,"x"))switch("l"!==t.labelpos&&"r"!==t.labelpos||(t.width-=t.labeloffset),t.labelpos){case"l":t.x-=t.width/2+t.labeloffset;break;case"r":t.x+=t.width/2+t.labeloffset}}))}(e))),n("    undoCoordinateSystem",(()=>D(e))),n("    translateGraph",(()=>function(e){var n=Number.POSITIVE_INFINITY,t=0,o=Number.POSITIVE_INFINITY,i=0,s=e.graph(),a=s.marginx||0,u=s.marginy||0;function d(e){var r=e.x,s=e.y,a=e.width,u=e.height;n=Math.min(n,r-a/2),t=Math.max(t,r+a/2),o=Math.min(o,s-u/2),i=Math.max(i,s+u/2)}r.A(e.nodes(),(function(n){d(e.node(n))})),r.A(e.edges(),(function(n){var t=e.edge(n);Object.prototype.hasOwnProperty.call(t,"x")&&d(t)})),n-=a,o-=u,r.A(e.nodes(),(function(t){var r=e.node(t);r.x-=n,r.y-=o})),r.A(e.edges(),(function(t){var i=e.edge(t);r.A(i.points,(function(e){e.x-=n,e.y-=o})),Object.prototype.hasOwnProperty.call(i,"x")&&(i.x-=n),Object.prototype.hasOwnProperty.call(i,"y")&&(i.y-=o)})),s.width=t-n+a,s.height=i-o+u}(e))),n("    assignNodeIntersects",(()=>function(e){r.A(e.edges(),(function(n){var t,r,o=e.edge(n),i=e.node(n.v),s=e.node(n.w);o.points?(t=o.points[0],r=o.points[o.points.length-1]):(o.points=[],t=s,r=i),o.points.unshift(L(i,t)),o.points.push(L(s,r))}))}(e))),n("    reversePoints",(()=>function(e){r.A(e.edges(),(function(n){var t=e.edge(n);t.reversed&&t.points.reverse()}))}(e))),n("    acyclic.undo",(()=>function(e){r.A(e.edges(),(function(n){var t=e.edge(n);if(t.reversed){e.removeEdge(n);var r=t.forwardName;delete t.reversed,delete t.forwardName,e.setEdge(n.w,n.v,t,r)}}))}(e)))}(n,t))),t("  updateInputGraph",(()=>function(e,n){r.A(e.nodes(),(function(t){var r=e.node(t),o=n.node(t);r&&(r.x=o.x,r.y=o.y,n.children(t).length&&(r.width=o.width,r.height=o.height))})),r.A(e.edges(),(function(t){var r=e.edge(t),o=n.edge(t);r.points=o.points,Object.prototype.hasOwnProperty.call(o,"x")&&(r.x=o.x,r.y=o.y)})),e.graph().width=n.graph().width,e.graph().height=n.graph().height}(e,n)))}))}var qe=["nodesep","edgesep","ranksep","marginx","marginy"],Ye={ranksep:50,edgesep:20,nodesep:50,rankdir:"tb"},ze=["acyclicer","ranker","rankdir","align"],Je=["width","height"],Ze={width:0,height:0},He=["minlen","weight","width","height","labeloffset"],Ke={minlen:1,weight:1,width:0,height:0,labeloffset:10,labelpos:"r"},Qe=["labelpos"];function Ue(e,n){return E.A(b.A(e,n),Number)}function We(e){var n={};return r.A(e,(function(e,t){n[t.toLowerCase()]=e})),n}}}]);