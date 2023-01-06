(ns reflet.css.bundled) (def css "@import url(\"https://fonts.googleapis.com/css2?family=Source+Sans+Pro:ital,wght@0,200;0,400;0,600;1,400&display=fallback\");\n@import url(\"https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,400;0,600;1,400&display=fallback\");\n.reflet-overlay {\n  all: initial;\n  display: block;\n  margin: 0;\n  line-height: 1.5;\n  color: #525252;\n  font-family: \"Source Sans Pro\", -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";\n  font-size: 1rem;\n  -webkit-text-size-adjust: 100%;\n  -webkit-tap-highlight-color: rgba(82, 82, 82, 0);\n}\n\n.reflet-tap {\n  display: none;\n}\n\n.reflet-node {\n  position: fixed;\n  visibility: hidden;\n  opacity: 0;\n  transition: all 0.3s ease;\n}\n.reflet-marks-on .reflet-node {\n  visibility: visible;\n  opacity: 1;\n}\n\n.reflet-glyph {\n  display: flex;\n  align-items: center;\n}\n\n.reflet-context {\n  display: flex;\n  position: absolute;\n  padding: 0.5rem 1rem;\n  border-radius: 0;\n  max-width: 500px;\n  background: rgba(248, 250, 255, 0.96);\n  box-shadow: 4px 4px rgba(208, 208, 208, 0.5);\n  font-size: 0.7rem;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n}\n\n.reflet-global-control {\n  display: flex;\n  position: fixed;\n  bottom: 1rem;\n  right: 1rem;\n  padding: 0.5rem 1rem;\n  border-radius: 0;\n  background: rgba(248, 250, 255, 0.96);\n  font-size: 0.8rem;\n  box-shadow: 4px 4px rgba(208, 208, 208, 0.5);\n  user-select: none;\n  text-transform: uppercase;\n  cursor: pointer;\n  visibility: hidden;\n  opacity: 0;\n  transition: all 0.3s ease;\n}\n.reflet-marks-on .reflet-global-control {\n  visibility: visible;\n  opacity: 1;\n}\n.reflet-global-control .reflet-control, .reflet-global-control svg {\n  width: 1rem;\n}\n.reflet-global-control:hover {\n  background: rgba(243, 246, 255, 0.96);\n}\n\n.reflet-mark-list {\n  position: absolute;\n  display: grid;\n  grid-template-columns: auto auto;\n  grid-template-rows: 32px;\n  align-items: start;\n  border-radius: 0;\n  background: rgba(248, 250, 255, 0.96);\n  font-size: 0.7rem;\n  box-shadow: 4px 4px rgba(208, 208, 208, 0.5);\n}\n.reflet-mark-list > div {\n  display: contents;\n  cursor: pointer;\n}\n.reflet-mark-list > div > * {\n  padding: 8px;\n  height: 100%;\n}\n.reflet-mark-list > div:hover > * {\n  background: rgba(243, 246, 255, 0.96);\n}\n.reflet-mark-list > div > :nth-child(2n+1) {\n  color: #7732ff;\n  font-weight: 600;\n}\n.reflet-mark-list > div > :nth-child(2n) {\n  cursor: pointer;\n  color: #525252;\n  padding-right: 1rem;\n}\n.reflet-mark-list > * {\n  white-space: nowrap;\n}\n\n.reflet-mark, .reflet-mark-group {\n  position: relative;\n  transform: translate(8px, 8px);\n  user-select: none;\n  color: #7732ff;\n  transition: color 0.3s ease;\n}\n.reflet-mark svg, .reflet-mark-group svg {\n  width: 16px;\n  margin-bottom: -8px;\n}\n.reflet-mark svg path, .reflet-mark-group svg path {\n  transition: all 0.3s linear;\n  opacity: 1;\n  visibility: visible;\n}\n@supports selector(:nth-child(1 of x)) {\n  .reflet-mark svg path, .reflet-mark-group svg path {\n    transition: none !important;\n  }\n}\n.reflet-mark .reflet-mark-list, .reflet-mark-group .reflet-mark-list {\n  margin-left: -8px;\n  margin-top: -8px;\n  transition: all 0.3s linear;\n  visibility: hidden;\n  opacity: 0;\n}\n@supports selector(:nth-child(1 of x)) {\n  .reflet-mark .reflet-mark-list, .reflet-mark-group .reflet-mark-list {\n    transition: none !important;\n  }\n}\n.reflet-mark .reflet-mark-list > div > :nth-child(2n+1), .reflet-mark-group .reflet-mark-list > div > :nth-child(2n+1) {\n  padding-left: 0.5rem;\n}\n.reflet-mark:hover svg path, .reflet-mark-group:hover svg path {\n  opacity: 0;\n  visibility: hidden;\n}\n.reflet-mark:hover .reflet-mark-list, .reflet-mark-group:hover .reflet-mark-list {\n  visibility: visible;\n  opacity: 1;\n}\n\n.reflet-panel {\n  position: fixed;\n  width: 18rem;\n  min-width: 3rem;\n  min-height: 3rem;\n  border: 2px solid #6b6b6b;\n  border-radius: 0;\n  box-shadow: 0 0 80px rgba(0, 0, 0, 0.07);\n  transform-style: preserve-3d;\n  color: #454545;\n  font-size: 0.7rem;\n  background: #ffffff;\n}\n.reflet-panel-shadow {\n  position: absolute;\n  width: 100%;\n  height: 100%;\n  top: 6px;\n  left: 6px;\n  border-radius: 0;\n  background: rgba(208, 208, 208, 0.5);\n  transform: translateZ(-1px);\n}\n.reflet-panel-handle {\n  position: absolute;\n  bottom: 0;\n  right: 0;\n  width: 16px;\n  height: 16px;\n  transition: all 0.3s ease;\n  opacity: 0;\n  color: #6b6b6b;\n}\n.reflet-panel-handle:hover, .reflet-panel:hover.reflet-resize .reflet-panel-handle {\n  opacity: 1;\n  cursor: nwse-resize;\n}\n.reflet-panel.reflet-resize {\n  cursor: nwse-resize;\n}\n\n.reflet-header {\n  display: grid;\n  grid-template-columns: 1fr auto 1fr;\n  grid-column-gap: 0.5rem;\n  position: sticky;\n  top: 0;\n  left: 0;\n  padding: 0.25rem 0.75rem;\n  border-top-left-radius: 0;\n  border-top-right-radius: 0;\n  border-bottom: 1px solid #6b6b6b;\n  background: #ffffff;\n  z-index: 1;\n  cursor: default;\n  user-select: none;\n}\n.reflet-move .reflet-header {\n  cursor: grabbing;\n}\n.reflet-header > :first-child:not(.reflet-props-title) {\n  display: flex;\n}\n.reflet-header > :first-child:not(.reflet-props-title) > :nth-child(2) {\n  text-transform: uppercase;\n}\n.reflet-header > :first-child:not(.reflet-props-title) > * {\n  white-space: nowrap;\n}\n\n.reflet-props-title, .reflet-ref-title {\n  display: flex;\n  overflow: hidden;\n  white-space: nowrap;\n  font-weight: 600;\n  grid-column: 2/span 1;\n}\n.reflet-props-title > :nth-child(n+2), .reflet-ref-title > :nth-child(n+2) {\n  margin-left: 0.25rem;\n  font-weight: 400;\n  font-style: italic;\n}\n\n.reflet-props-title > :last-child {\n  overflow: hidden;\n  text-overflow: ellipsis;\n  direction: rtl;\n}\n\n.reflet-ref-title > .reflet-ref {\n  color: #525252;\n}\n\n.reflet-close {\n  justify-self: end;\n  grid-column: 3/span 1;\n}\n\n.reflet-control, .reflet-close {\n  width: 0.5rem;\n  transition: all 0.3s ease;\n  cursor: pointer;\n  color: #6b6b6b;\n  flex: 0 0 auto;\n  user-select: none;\n}\n.reflet-control.reflet-glyph:not(:last-child), .reflet-close.reflet-glyph:not(:last-child) {\n  margin-right: 0.5rem;\n}\n.reflet-control:hover, .reflet-close:hover {\n  color: #454545;\n}\n\n.reflet-refs {\n  display: flex;\n  flex-direction: column;\n}\n\n.reflet-content {\n  display: flex;\n  flex-direction: column;\n  width: 100%;\n  height: 100%;\n  overflow-y: scroll;\n}\n.reflet-content > *:not(.reflet-header) {\n  padding: 0.5rem 0.75rem;\n}\n.reflet-content > :not(:first-child, :nth-child(2)) {\n  border-top: 1px solid #6b6b6b;\n}\n\n.reflet-set-lens {\n  display: flex;\n  justify-content: space-evenly;\n  user-select: none;\n}\n.reflet-set-lens > * {\n  cursor: pointer;\n  font-size: 0.7rem;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n  text-transform: uppercase;\n}\n\n.reflet-event-lens, .reflet-query-lens, .reflet-fsm-lens {\n  display: grid;\n  grid-template-columns: min-content auto;\n  column-gap: 0.5rem;\n}\n.reflet-event-lens > div:first-child > .reflet-divider, .reflet-query-lens > div:first-child > .reflet-divider, .reflet-fsm-lens > div:first-child > .reflet-divider {\n  display: none;\n}\n.reflet-event-lens > div, .reflet-query-lens > div, .reflet-fsm-lens > div {\n  display: contents;\n}\n.reflet-event-lens > div > .reflet-divider, .reflet-query-lens > div > .reflet-divider, .reflet-fsm-lens > div > .reflet-divider {\n  grid-column: 1/span 2;\n  border-top: 1px dotted #6b6b6b;\n  margin: 0.5rem -0.75rem;\n}\n.reflet-event-lens > div > :nth-child(n+3), .reflet-query-lens > div > :nth-child(n+3), .reflet-fsm-lens > div > :nth-child(n+3) {\n  grid-column: 2;\n  overflow: hidden;\n}\n.reflet-event-lens .reflet-coll-data, .reflet-query-lens .reflet-coll-data, .reflet-fsm-lens .reflet-coll-data {\n  display: flex;\n  flex-direction: row;\n}\n.reflet-event-lens .reflet-coll-data > *:not(:first-child), .reflet-query-lens .reflet-coll-data > *:not(:first-child), .reflet-fsm-lens .reflet-coll-data > *:not(:first-child) {\n  margin-left: 0.5rem;\n  flex-shrink: 0.35;\n}\n\n.reflet-query-lens > div .reflet-query-control, .reflet-fsm-lens > div .reflet-query-control {\n  display: flex;\n  align-items: flex-start;\n  grid-column: 1;\n  padding-top: 0.25rem;\n}\n.reflet-query-lens > div .reflet-query-control .reflet-control:hover path, .reflet-fsm-lens > div .reflet-query-control .reflet-control:hover path {\n  fill: currentColor;\n}\n.reflet-query-lens > div .reflet-query-control .reflet-glyph:not(:last-child), .reflet-fsm-lens > div .reflet-query-control .reflet-glyph:not(:last-child) {\n  margin-right: 0.25rem;\n}\n.reflet-query-lens > div .reflet-query-control > :nth-child(2), .reflet-fsm-lens > div .reflet-query-control > :nth-child(2) {\n  transform: scaleX(-1);\n}\n.reflet-query-lens > div .reflet-query-control .reflet-control-disabled, .reflet-fsm-lens > div .reflet-query-control .reflet-control-disabled {\n  color: #b8b8b8;\n}\n.reflet-query-lens .reflet-map-data, .reflet-fsm-lens .reflet-map-data {\n  flex: unset;\n}\n.reflet-query-lens svg text, .reflet-fsm-lens svg text {\n  font-size: 8px;\n}\n\n.reflet-query-lens > div > :not(.reflet-map):last-child > .reflet-coll-data {\n  flex-direction: column;\n}\n\n.reflet-transition > :first-child {\n  display: flex;\n  overflow: hidden;\n  justify-content: center;\n  font-style: italic;\n}\n.reflet-transition > :first-child > :first-child {\n  flex: 1 1 0;\n}\n.reflet-transition > :first-child > :nth-child(3) {\n  flex: 1 1 0;\n  display: flex;\n  justify-content: end;\n}\n.reflet-transition > :first-child > :nth-child(2) {\n  margin: 0 0.75rem;\n}\n.reflet-transition > :first-child > .reflet-keyword > * {\n  color: #3733ff;\n}\n.reflet-transition > :nth-child(n+2) {\n  display: grid;\n  grid-template-columns: fit-content(3rem) auto;\n  grid-column-gap: 0.5rem;\n}\n.reflet-transition > :nth-child(n+2) > :nth-child(2n+1) {\n  color: #818181;\n}\n\n.reflet-fsm-init-state {\n  width: 100%;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n  text-transform: uppercase;\n  color: #818181;\n}\n\n.reflet-map {\n  display: flex;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n}\n.reflet-map::before, .reflet-map::after {\n  display: flex;\n  width: 0.5rem;\n  flex-shrink: 0;\n  font-weight: 700;\n  white-space: nowrap;\n}\n.reflet-map::before {\n  justify-content: flex-start;\n  align-self: start;\n}\n.reflet-map::after {\n  justify-content: flex-end;\n  align-self: end;\n}\n\n.reflet-map-data {\n  display: grid;\n  grid-template-columns: auto auto;\n  column-gap: 0.5rem;\n  flex: 1;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n  color: #525252;\n}\n.reflet-map-data > div {\n  display: contents;\n}\n.reflet-map-data > div > * {\n  display: flex;\n  overflow: hidden;\n  white-space: nowrap;\n}\n.reflet-map-data > div > :nth-child(2) {\n  justify-content: flex-end;\n}\n\n.reflet-vec, .reflet-set, .reflet-list {\n  display: flex;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n  overflow: hidden;\n}\n.reflet-vec::before, .reflet-vec::after, .reflet-set::before, .reflet-set::after, .reflet-list::before, .reflet-list::after {\n  display: flex;\n  width: 0.5rem;\n  flex-shrink: 0;\n  font-weight: 700;\n  white-space: nowrap;\n}\n.reflet-vec::before, .reflet-set::before, .reflet-list::before {\n  justify-content: flex-start;\n  align-self: start;\n}\n.reflet-vec::after, .reflet-set::after, .reflet-list::after {\n  justify-content: flex-end;\n  align-self: end;\n}\n\n.reflet-coll-data {\n  display: flex;\n  flex-direction: column;\n  overflow: hidden;\n  white-space: nowrap;\n}\n.reflet-coll-data > * {\n  display: flex;\n  overflow: hidden;\n  white-space: nowrap;\n}\n\n.reflet-map::before {\n  content: \"{\";\n}\n\n.reflet-map::after {\n  content: \"}\";\n}\n\n.reflet-vec::before {\n  content: \"[\";\n}\n\n.reflet-vec::after {\n  content: \"]\";\n}\n\n.reflet-set::before {\n  content: \"#{\";\n  width: 0.85rem;\n}\n\n.reflet-set::after {\n  content: \"}\";\n}\n\n.reflet-list::before {\n  content: \"(\";\n}\n\n.reflet-list::after {\n  content: \")\";\n}\n\n.reflet-coll-expander {\n  user-select: none;\n  cursor: pointer;\n}\n.reflet-coll-expander > *, .reflet-coll-expander svg {\n  display: flex;\n  transition: all 0.3s ease;\n  white-space: nowrap;\n}\n.reflet-coll-expander > :nth-child(2) {\n  flex: 1 0 0;\n  overflow: hidden;\n}\n.reflet-coll-expander > :nth-child(2) > * {\n  overflow: visible;\n  transition: all 0.3s ease;\n}\n.reflet-coll-expander > :nth-child(2) > *:not(:last-child) {\n  margin-right: 0.5rem;\n}\n.reflet-coll-expand .reflet-coll-expander > :nth-child(2) > * {\n  opacity: 0.1;\n}\n.reflet-coll-expand .reflet-coll-expander svg {\n  transform: scale(1, -1);\n}\n.reflet-coll-expander > :first-child, .reflet-coll-expander > :last-child {\n  font-weight: 700;\n}\n.reflet-coll-expand .reflet-coll-expander > :first-child, .reflet-coll-expand .reflet-coll-expander > :last-child {\n  opacity: 0.2;\n}\n.reflet-coll-expander .reflet-glyph {\n  margin: 0 0.5rem;\n  padding-top: 1px;\n}\n.reflet-coll-expander svg {\n  width: 12px;\n}\n\n.reflet-coll-expanded > * {\n  grid-column-start: 1;\n  grid-column-end: span 2;\n}\n\n.reflet-ref {\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n  color: #ff00e9;\n  display: inline;\n  overflow: hidden;\n  text-overflow: ellipsis;\n  min-width: 0.25rem;\n}\n:not(.reflet-ref-title) > .reflet-ref {\n  cursor: pointer;\n}\n\n.reflet-keyword {\n  display: flex;\n  overflow: hidden;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n  white-space: nowrap;\n}\n.reflet-keyword > :last-child {\n  display: inline;\n  overflow: hidden;\n  text-overflow: ellipsis;\n  min-width: 0.25rem;\n  direction: rtl;\n}\n\n.reflet-string {\n  color: #818181;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n  font-style: italic;\n}\n.reflet-string > * {\n  display: inline;\n  overflow: hidden;\n  text-overflow: ellipsis;\n  min-width: 0.25rem;\n}\n\n.reflet-nil {\n  color: #818181;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n}\n\n.reflet-no-data {\n  display: flex;\n  justify-content: center;\n  width: 100%;\n  text-transform: uppercase;\n}\n\n.reflet-number {\n  color: #8b8ab8;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n}\n\n.reflet-html {\n  color: #818181;\n  font-family: \"IBM Plex Mono\", \"Fira Mono\";\n}\n\n/*# sourceMappingURL=debug.css.map */"
)