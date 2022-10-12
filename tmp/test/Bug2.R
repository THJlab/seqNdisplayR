###

# THE BUG

library(seqNdisplayR)

xl_fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test1.xlsx'

session = load_excel(xl_fname, load_annotations = T)

plot(session, feature='LMO4')

session[['parameters']][['3-seq']][['calcMean']] = FALSE

plot(session, feature='LMO4')


###

# EMPTY SHEET

xl_fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test2.xlsx'

session = load_excel(xl_fname, load_annotations = T)

fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test2.xlsx'


run_seqNdisplayR_app()

xl_fname = "/Library/Frameworks/R.framework/Versions/4.0/Resources/library/seqNdisplayR/extdata/minimal_example_excel_template2.xlsx"

session = load_excel(xl_fname, load_annotations = T)


###

# WRONGLY FILLED SAMPLES

xl_fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test3.xlsx'

session = load_excel(xl_fname, load_annotations = T)


###

# MISSING COLOR COLUMN

xl_fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test4.xlsx'

session = load_excel(xl_fname, load_annotations = T)

plot(session, feature='LMO4')

###

# COLOR COLUMN

xl_fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test5.xlsx'

session = load_excel(xl_fname, load_annotations = T)

plot(session, feature='LMO4')


###

# TOP ROW VALUES

xl_fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test6.xlsx'

session = load_excel(xl_fname, load_annotations = T)

plot(session, feature='LMO4')


###

# extension to file name

xl_fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test7'

session = load_excel(xl_fname, load_annotations = T)

plot(session, feature='LMO4')


xl_fname = '/Users/au103725/Library/CloudStorage/OneDrive-AarhusUniversitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/tmp/test/seqNdisplayR_sample_sheet_test8'

session = load_excel(xl_fname, load_annotations = T)

plot(session, feature='LMO4')


xl_fname <- system.file('extdata', 'seqNdisplayR_sample_sheet_elaborate2.xlsx', package='seqNdisplayR')
session = load_excel(xl_fname, load_annotations = T)
plot(session, feature='LMO4')
