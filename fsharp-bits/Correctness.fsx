type EmailAddress = EmailAddress of string

let sendEmail(EmailAddress email) =
    printfn "Sending the email %s" email

sendEmail (EmailAddress "rms@emacs.org")
