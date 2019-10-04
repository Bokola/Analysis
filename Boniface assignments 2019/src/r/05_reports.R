cat('Reporting ....\n\n')
#add plots
# doc = docx()
# options("ReporteRs-fontsize" = 12)
# doc<- addTitle(doc, value= "Figure 1: Diagnosis of Diabetes and Hypertension by Sub- county")
# addPlot(doc, fun = print, x= diag_plot)#, fontname = "Times New Roman", width = 4, height = 4)
# doc<- addTitle(doc, value= "Figure 2: Bar graph of major complications by sub-county")
# addPlot(doc, fun = print, x= complications_plot)
# doc<- addTitle(doc, value= "Figure 3: Bar graph of a combination of most prescribed treatments by sub-county")
# addPlot(doc, fun = print, x= treatment_plot)
# # doc<- addTitle(doc, value= "Figure 4: Line graph of total dm patients in care by sub-county")
# # addPlot(doc, fun = print, x= p1)
# # doc<- addTitle(doc, value= "Figure 5: Line graph of total htn patients in care by sub-county")
# # addPlot(doc, fun = print, x= p2)
# # doc<- addTitle(doc, value= "Figure 8: Line graph of dm revisits by sub-county")
# # addPlot(doc, fun = print, x= p3)
# # doc<- addTitle(doc, value= "Figure 7: Line graph of dm first visit by sub-county")
# # addPlot(doc, fun = print, x= p4)



# Report Genaration
# Word Format

options("ReporteRs-fontsize" = 8)

doc = docx() %>% 
  # Add spaces to center the cover page
  addParagraph(., c("\n")) %>% addParagraph(., c("\n")) %>% addParagraph(., c("\n")) %>%
  addParagraph(., c("\n")) %>% addParagraph(., c("\n")) %>% addParagraph(., c("\n")) %>%
  addParagraph(., c("\n")) %>% addParagraph(., c("\n")) %>% addParagraph(., c("\n")) %>%
  addParagraph(., c("\n")) %>% addParagraph(., c("\n")) %>% addParagraph(., c("\n")) %>%
  
  addParagraph(., pot("Tables of Treatments and Complications:",  textProperties(font.size = 18)), 
               par.properties = parProperties(text.align = "center")) %>%
  addParagraph(., c("")) %>%
  addParagraph(., c("")) %>%
  addPageBreak(.) %>%
  addTitle(., "Table of Contents", style = "Titre10") %>%
  addTOC(.) %>%
  addPageBreak(.) 

doc = addSection(doc, landscape = TRUE) # Change Orientation of the page

# Add tables



table1 = FlexTable( data = complications_table, add.rownames = F,
                    header.cell.props = cellProperties( background.color = "black" ),
                    header.text.props = textProperties( color = "gray",
                                                        font.size = 8, font.weight = "bold" ),
                    body.text.props = textProperties( font.size = 8 )
)

table1 = setZebraStyle(table1,odd = '#eeeeee', even = 'white')

doc<- addTitle(doc, value= "Table 1: omplications")
doc = addFlexTable( doc, flextable = table1) %>%  addParagraph(., c("")) # 2 line breaks

table2 = FlexTable( data = treatments_table, add.rownames = F,
                    header.cell.props = cellProperties( background.color = "black" ),
                    header.text.props = textProperties( color = "gray",
                                                        font.size = 8, font.weight = "bold" ),
                    body.text.props = textProperties( font.size = 8 )
)

table2 = setZebraStyle(table2,odd = '#eeeeee', even = 'white')

doc<- addTitle(doc, value= "Table 2: Treatment combinations")
doc = addFlexTable( doc, flextable = table2) %>%  addParagraph(., c("")) # 2 line breaks

doc = addSection(doc, landscape = TRUE) # Change Orientation of the page


# save report
writeDoc(doc, file = file.path(cache_dir, paste("Tables of treatments and complications -", 
                                                      format(Sys.Date(),"%b %Y"), "to", format(Sys.Date(),"%b %Y"),".docx")))
cat("\t\tWord Report ready! \n\t\t\tsaved in:", cache_dir, "\n\n")



writeDoc(doc, file = file.path(cache_dir, "diagnosis_plots.docx"))

cat('Report contained in ', cache_dir,'\n')
